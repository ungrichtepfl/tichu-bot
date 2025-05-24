#include "game.h"
#include "jsmn.h"
#include <assert.h>
#include <raylib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#if defined(__EMSCRIPTEN__) || defined(__wasm__) || defined(__wasm32__) ||     \
    defined(__wasm64__)
#define WASM 1
#else
#define WASM 0
#endif

#define LENGTH(a) (sizeof(a) / sizeof(a[0]))

#define WIN_WIDTH 800
#define WIN_HEIHT WIN_WIDTH
#define FPS 60

#define ASSET_BLACK_POSTFIX "a.png"
#define ASSET_BLUE_POSTFIX "b.png"
#define ASSET_GREEN_POSTFIX "c.png"
#define ASSET_RED_POSTFIX "d.png"

#define ASSET_DRAGON "drache.png"
#define ASSET_DOG "hund.png"
#define ASSET_MAHJONG "mahjong.png"
#define ASSET_PHOENIX "phoenix.png"
#define ASSET_BACKGROUND "background.png"

#define ASSET_PATH "./gui/images/"
#define CARD_ASSET_REL_PATH "cards/"

#define NUM_JSON_TOKENS (1 << 10)
jsmn_parser json_parser;
jsmntok_t json_tokens[NUM_JSON_TOKENS];

typedef struct {
  Texture2D red[CARDS_PER_COLOR];
  Texture2D blue[CARDS_PER_COLOR];
  Texture2D green[CARDS_PER_COLOR];
  Texture2D black[CARDS_PER_COLOR];
  Texture2D dragon;
  Texture2D mahjong;
  Texture2D phoenix;
  Texture2D dog;
  Texture2D background;
} Assets;

typedef struct {
  Vector2 pos;
  float rot;
  float scale;
} CardPose;

typedef struct {
  CardPose card_pose[TOTAL_CARDS];
  size_t render_prio[TOTAL_CARDS];
  int selected_piece_idx;
} State;

State g_state = {0};

void set_highest_prio(size_t idx) {
  int cut = -1;
  for (int i = 0; (size_t)i < LENGTH(g_state.render_prio); ++i) {
    if (g_state.render_prio[i] == idx) {
      cut = i;
      break;
    }
  }
  if (cut == -1) {
    fprintf(stderr, "Could not set highest prio, idx not found. Most probably "
                    "something wrong in the init function.");
    return;
  }
  memmove(&g_state.render_prio[1], &g_state.render_prio[0],
          cut * sizeof(g_state.render_prio[0]));
  g_state.render_prio[0] = idx;
}

size_t get_card_index(Card card) {
  if (card.color < NUM_COLORS)
    return card.color * CARDS_PER_COLOR + card.number - 2;
  else
    return card.color - NUM_COLORS + CARDS_PER_COLOR * NUM_COLORS;
}

CardPose get_card_pose(Card card) {
  return g_state.card_pose[get_card_index(card)];
}

Card get_card_from_index(size_t index) {
  long long idx = (long long)index;

  if (idx < CARDS_PER_COLOR * NUM_COLORS)
    return (Card){
        .color = idx / CARDS_PER_COLOR,
        .number = idx % CARDS_PER_COLOR + 2,
    };
  else
    return (Card){
        .color = idx - CARDS_PER_COLOR * NUM_COLORS + NUM_COLORS,
        .number = 0,
    };
}

Assets g_assets = {0};

void load_global_assets(void) {

  char asset_path[256];
  Texture2D asset;
  for (size_t i = 0; i < CARDS_PER_COLOR; ++i) {
    char num[4];
    sprintf(num, "%02zu", i + 2); // Assets starts with a 2

    // BLACK ASSETS
    sprintf(asset_path, "%s%s", ASSET_PATH CARD_ASSET_REL_PATH, num);
    strncat(asset_path, ASSET_BLACK_POSTFIX, sizeof(asset_path) - 1);
    asset = LoadTexture(asset_path);
    g_assets.black[i] = asset;

    // BLUE ASSETS
    sprintf(asset_path, "%s%s", ASSET_PATH CARD_ASSET_REL_PATH, num);
    strncat(asset_path, ASSET_BLUE_POSTFIX, sizeof(asset_path) - 1);
    asset = LoadTexture(asset_path);
    g_assets.blue[i] = asset;

    // RED ASSETS
    sprintf(asset_path, "%s%s", ASSET_PATH CARD_ASSET_REL_PATH, num);
    strncat(asset_path, ASSET_RED_POSTFIX, sizeof(asset_path) - 1);
    asset = LoadTexture(asset_path);
    g_assets.red[i] = asset;

    // GREEN ASSETS
    sprintf(asset_path, "%s%s", ASSET_PATH CARD_ASSET_REL_PATH, num);
    strncat(asset_path, ASSET_GREEN_POSTFIX, sizeof(asset_path) - 1);
    asset = LoadTexture(asset_path);
    g_assets.green[i] = asset;
  }

  // DRAGON
  sprintf(asset_path, "%s", ASSET_PATH CARD_ASSET_REL_PATH);
  strncat(asset_path, ASSET_DRAGON, sizeof(asset_path) - 1);
  asset = LoadTexture(asset_path);
  g_assets.dragon = asset;

  // PHOENIX
  sprintf(asset_path, "%s", ASSET_PATH CARD_ASSET_REL_PATH);
  strncat(asset_path, ASSET_PHOENIX, sizeof(asset_path) - 1);
  asset = LoadTexture(asset_path);
  g_assets.phoenix = asset;

  // DOG
  sprintf(asset_path, "%s", ASSET_PATH CARD_ASSET_REL_PATH);
  strncat(asset_path, ASSET_DOG, sizeof(asset_path) - 1);
  asset = LoadTexture(asset_path);
  g_assets.dog = asset;

  // MAHJONG
  sprintf(asset_path, "%s", ASSET_PATH CARD_ASSET_REL_PATH);
  strncat(asset_path, ASSET_MAHJONG, sizeof(asset_path) - 1);
  asset = LoadTexture(asset_path);
  g_assets.mahjong = asset;

  // BACKGROUND
  sprintf(asset_path, "%s", ASSET_PATH);
  strncat(asset_path, ASSET_BACKGROUND, sizeof(asset_path) - 1);
  asset = LoadTexture(asset_path);
  g_assets.background = asset;
}

void unload_global_assets(void) {
  for (size_t i = 0; i < CARDS_PER_COLOR; ++i) {
    UnloadTexture(g_assets.green[i]);
    UnloadTexture(g_assets.blue[i]);
    UnloadTexture(g_assets.black[i]);
    UnloadTexture(g_assets.red[i]);
  }
  UnloadTexture(g_assets.dragon);
  UnloadTexture(g_assets.dog);
  UnloadTexture(g_assets.mahjong);
  UnloadTexture(g_assets.phoenix);
  UnloadTexture(g_assets.background);
}
Texture2D get_background_asset(void) { return g_assets.background; }

Texture2D get_card_asset(Card card) {
  int card_num = card.number - 2; // Starts at 2
  switch (card.color) {
  case RED_CARD: {
    assert(card_num < CARDS_PER_COLOR && "Wrong card number!");
    return g_assets.red[card_num];
  } break;
  case BLUE_CARD: {
    assert(card_num < CARDS_PER_COLOR && "Wrong card number!");
    return g_assets.blue[card_num];
  } break;
  case BLACK_CARD: {
    assert(card_num < CARDS_PER_COLOR && "Wrong card number!");
    return g_assets.black[card_num];
  } break;
  case GREEN_CARD: {
    assert(card_num < CARDS_PER_COLOR && "Wrong card number!");
    return g_assets.green[card_num];
  } break;
  case DOG: {
    return g_assets.dog;
  } break;
  case DRAGON: {
    return g_assets.dragon;
  } break;
  case MAHJONG: {
    return g_assets.mahjong;
  } break;
  case PHOENIX: {
    return g_assets.phoenix;
  } break;
  default:
    assert(0 && "Unreachable");
  }
}

bool is_mouse_down(void) {
#if WASM
  return mouse_down > 0;
#else
  return IsMouseButtonDown(MOUSE_BUTTON_LEFT);
#endif
}

Vector2 get_mouse_position(void) {
#if WASM
  return (Vector2){.x = (float)mouse_x, .y = (float)mouse_y};
#else
  return GetMousePosition();
#endif
}

Rectangle get_card_rectangle(Card card) {
  Texture2D card_texture = get_card_asset(card);
  CardPose pose = get_card_pose(card);
  return (Rectangle){
      .x = pose.pos.x,
      .y = pose.pos.y,
      .width = card_texture.width * pose.scale,
      .height = card_texture.height * pose.scale,
  };
}

void update_card_position(void) {
  static Vector2 previous_mouse_touch = {0};
  if (is_mouse_down()) {
    Vector2 mouse_touch = get_mouse_position();
    if (g_state.selected_piece_idx >= 0) {
      g_state.card_pose[g_state.selected_piece_idx].pos.x +=
          mouse_touch.x - previous_mouse_touch.x;
      g_state.card_pose[g_state.selected_piece_idx].pos.y +=
          mouse_touch.y - previous_mouse_touch.y;
    } else {
      for (size_t i = 0; i < LENGTH(g_state.render_prio); ++i) {
        size_t idx = g_state.render_prio[i];
        Card card = get_card_from_index(idx);
        Rectangle card_rec = get_card_rectangle(card);
        if (CheckCollisionPointRec(mouse_touch, card_rec)) {
          g_state.selected_piece_idx = idx;
          set_highest_prio(idx);
          break;
        }
      }
    }
    previous_mouse_touch = mouse_touch;
  } else {
    g_state.selected_piece_idx = -1;
  }
}

void setup_global_game_state(void) {
  g_state.selected_piece_idx = -1; // Nothing selected

  for (size_t i = 0; i < LENGTH(g_state.render_prio); ++i) {
    g_state.render_prio[i] = i;
  }

  for (size_t i = 0; i < LENGTH(g_state.card_pose); ++i) {
    int col = NUM_COLORS + 1;
    // No scale
    g_state.card_pose[i].scale = 1.f;
    // No rotation
    g_state.card_pose[i].rot = 0.f;

    // Evenly space the cards
    g_state.card_pose[i].pos.x = (i % col) * ((float)WIN_WIDTH / col);
    g_state.card_pose[i].pos.y = ((float)i / col) * ((float)WIN_HEIHT / col);
    // Set the last 4 cards to be in the middle
  }
}

void draw_cards(void) {
  for (int i = (int)LENGTH(g_state.render_prio) - 1; i >= 0; --i) {
    size_t idx = g_state.render_prio[i];
    Card card = get_card_from_index(idx);
    CardPose pose = g_state.card_pose[idx];
    Texture2D card_texture = get_card_asset(card);
    DrawTextureEx(card_texture, pose.pos, pose.rot, pose.scale, WHITE);
  }
}

void setup_global_json_parser(void) { jsmn_init(&json_parser); }

void init(void) {
  setup_global_game_state();
  setup_global_json_parser();
  InitWindow(WIN_WIDTH, WIN_HEIHT, "Tichu");
  load_global_assets();
#if !WASM
  SetTargetFPS(FPS);
#endif
}

void deinit(void) {
  unload_global_assets();
  CloseWindow();
}

void update_draw(void) {
  update_card_position();

  BeginDrawing();
  ClearBackground(WHITE);
  DrawTexture(get_background_asset(), 0, 0, WHITE);
  draw_cards();

  EndDrawing();
}

const char *test_json =
    "[{\"board\":[],\"currentDealer\":\"P4\",\"finishOrder\":[],\"gameConfig\":"
    "{\"scoreLimit\":1000,\"sittingOrder\":[\"P1\",\"P2\",\"P3\",\"P4\"],"
    "\"teamNames\":[\"Team 1\",\"Team "
    "2\"]},\"gamePhase\":{\"contents\":[{\"contents\":[\"Queen\",\"Clubs\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"King\",\"Clubs\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Three\",\"Spades\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"King\",\"Diamonds\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Eight\",\"Spades\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Four\",\"Diamonds\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Three\","
    "\"Hearts\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ace\",\"Hearts\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Five\",\"Hearts\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Two\",\"Diamonds\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"King\",\"Spades\"],\"tag\":\"PokerCard\"},{\"contents\":"
    "[\"Queen\",\"Diamonds\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Six\","
    "\"Clubs\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ace\",\"Diamonds\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\",\"Spades\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Eight\",\"Hearts\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"Nine\",\"Spades\"],\"tag\":\"PokerCard\"},{\"contents\":"
    "[\"Five\",\"Clubs\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ace\","
    "\"Spades\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Four\",\"Clubs\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Four\",\"Hearts\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"King\",\"Hearts\"],\"tag\":\"PokerCard\"},"
    "{\"contents\":[\"Nine\",\"Diamonds\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Three\",\"Diamonds\"],\"tag\":\"PokerCard\"},{\"tag\":"
    "\"Phoenix\"},{\"contents\":[\"Two\",\"Spades\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Queen\",\"Hearts\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Ten\",\"Clubs\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Three\","
    "\"Clubs\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Jack\",\"Hearts\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Five\",\"Spades\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Two\",\"Hearts\"],\"tag\":\"PokerCard\"},{"
    "\"tag\":\"Dog\"},{\"contents\":[\"Four\",\"Spades\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Five\",\"Diamonds\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Eight\",\"Clubs\"],\"tag\":\"PokerCard\"},"
    "{\"contents\":[\"Ten\",\"Spades\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Jack\",\"Diamonds\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ten\","
    "\"Diamonds\"],\"tag\":\"PokerCard\"},{\"tag\":\"Dragon\"},{\"contents\":["
    "\"Ace\",\"Clubs\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\","
    "\"Diamonds\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\",\"Hearts\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\",\"Clubs\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Six\",\"Spades\"],\"tag\":\"PokerCard\"},{"
    "\"tag\":\"Mahjong\"},{\"contents\":[\"Two\",\"Clubs\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Queen\",\"Spades\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"Eight\",\"Diamonds\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Jack\",\"Clubs\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Nine\",\"Hearts\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Jack\","
    "\"Spades\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Nine\",\"Clubs\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Six\",\"Hearts\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Ten\",\"Hearts\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Six\",\"Diamonds\"],\"tag\":\"PokerCard\"}],\"tag\":"
    "\"Dealing\"},\"generator\":[9044394885522251889,15525276302677374087],"
    "\"hands\":{\"P1\":[],\"P2\":[],\"P3\":[],\"P4\":[]},\"scores\":{\"Team "
    "1\":0,\"Team "
    "2\":0},\"shouldGameStop\":false,\"tichus\":{\"P1\":null,\"P2\":null,"
    "\"P3\":null,\"P4\":null},\"tricks\":{\"P1\":[],\"P2\":[],\"P3\":[],\"P4\":"
    "[]},\"winnerTeams\":[]},{\"P1\":[{\"tag\":\"Stop\"}],\"P2\":[{\"tag\":"
    "\"Stop\"}],\"P3\":[{\"tag\":\"Stop\"}],\"P4\":[{\"tag\":\"Stop\"}]}]\r\n";

void print_json_error(int err) {
  switch (err) {
  case JSMN_ERROR_NOMEM: {
    fprintf(stderr,
            "Not enough tokens were provided. There are more than the "
            "specified NUM_JSON_TOKENS=%d.\n",
            NUM_JSON_TOKENS);

  } break;
  case JSMN_ERROR_INVAL: {
    fprintf(stderr, "Invalid character inside JSON string\n");
  } break;
  case JSMN_ERROR_PART: {
    fprintf(stderr,
            "The string is not a full JSON packet, more bytes expected.\n");
  } break;
  default: {
    assert(0 && "Unreachable");
  } break;
  }
}

void parse_game(Game *game, const char *game_json) {

  int num_tokens = jsmn_parse(&json_parser, game_json, strlen(game_json),
                              json_tokens, NUM_JSON_TOKENS);
  if (num_tokens < 0) {
    print_json_error(num_tokens);
  }

  for (int i = 0; i < num_tokens; ++i) {
    // TODO: Finish function
    printf("Type: %d (start: %d, end: %d, size: %d)\n", json_tokens[i].type,
           json_tokens[i].start, json_tokens[i].end, json_tokens[i].size);
  }
  memset(game, 0, sizeof(*game));
}

#if !WASM
int main(void) {
  init();

  Game game = {0};
  parse_game(&game, test_json);

  while (!WindowShouldClose()) {
    update_draw();
  }

  deinit();
}
#endif
