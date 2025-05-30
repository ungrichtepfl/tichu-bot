#include "game.h"
#include "jsmn.h"
#include <assert.h>
#include <errno.h>
#include <raylib.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__EMSCRIPTEN__) || defined(__wasm__) || defined(__wasm32__) ||     \
    defined(__wasm64__)
#define WASM 1
#else
#define WASM 0
#endif

#define LENGTH(a) (sizeof(a) / sizeof(a[0]))
#define MAX(a, b) (a > b ? a : b)
#define MIN(a, b) (a < b ? a : b)

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
#define SAFECPY(g, t, n) strncpy(g, t, MIN(sizeof(g) - 1, (unsigned long)n))

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

bool json_str_equal(const char *json, jsmntok_t *tok, const char *s) {
  if (tok->type == JSMN_STRING && (int)strlen(s) == tok->end - tok->start &&
      strncmp(json + tok->start, s, tok->end - tok->start) == 0) {
    return true;
  }
  return false;
}

bool json_bool(const char *json, jsmntok_t *tok) {
  const char *t = "true";
  const char *f = "false";
  assert(tok->type == JSMN_PRIMITIVE && "Boolean must be a primitive.");
  if ((int)strlen(t) == tok->end - tok->start &&
      strncmp(json + tok->start, t, tok->end - tok->start) == 0) {
    return true;
  } else if ((int)strlen(f) == tok->end - tok->start &&
             strncmp(json + tok->start, f, tok->end - tok->start) == 0) {
    return false;
  } else {
    fprintf(stderr, "Not a boolean: %.*s\n", tok->end - tok->start,
            json + tok->start);
    assert(0);
  }
}

const char *test_json =
    "[{\"board\":[],\"currentDealer\":\"P4\",\"finishOrder\":[],\"gameConfig\":"
    "{\"scoreLimit\":1000,\"sittingOrder\":[\"P1\",\"P2\",\"P3\",\"P4\"],"
    "\"teamNames\":[\"Team 1\",\"Team "
    "2\"]},\"gamePhase\":{\"contents\":[{\"contents\":[\"Queen\",\"Red\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"King\",\"Red\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Three\",\"Black\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"King\",\"Blue\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Eight\",\"Black\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Four\",\"Blue\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Three\","
    "\"Green\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ace\",\"Green\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Five\",\"Green\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Two\",\"Blue\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"King\",\"Black\"],\"tag\":\"PokerCard\"},{\"contents\":"
    "[\"Queen\",\"Blue\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Six\","
    "\"Red\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ace\",\"Blue\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\",\"Black\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Eight\",\"Green\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"Nine\",\"Black\"],\"tag\":\"PokerCard\"},{\"contents\":"
    "[\"Five\",\"Red\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ace\","
    "\"Black\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Four\",\"Red\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Four\",\"Green\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"King\",\"Green\"],\"tag\":\"PokerCard\"},"
    "{\"contents\":[\"Nine\",\"Blue\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Three\",\"Blue\"],\"tag\":\"PokerCard\"},{\"tag\":"
    "\"Phoenix\"},{\"contents\":[\"Two\",\"Black\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Queen\",\"Green\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Ten\",\"Red\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Three\","
    "\"Red\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Jack\",\"Green\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Five\",\"Black\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Two\",\"Green\"],\"tag\":\"PokerCard\"},{"
    "\"tag\":\"Dog\"},{\"contents\":[\"Four\",\"Black\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Five\",\"Blue\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Eight\",\"Red\"],\"tag\":\"PokerCard\"},"
    "{\"contents\":[\"Ten\",\"Black\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Jack\",\"Blue\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Ten\","
    "\"Blue\"],\"tag\":\"PokerCard\"},{\"tag\":\"Dragon\"},{\"contents\":["
    "\"Ace\",\"Red\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\","
    "\"Blue\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\",\"Green\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Seven\",\"Red\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Six\",\"Black\"],\"tag\":\"PokerCard\"},{"
    "\"tag\":\"Mahjong\"},{\"contents\":[\"Two\",\"Red\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Queen\",\"Black\"],\"tag\":\"PokerCard\"}"
    ",{\"contents\":[\"Eight\",\"Blue\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Jack\",\"Red\"],\"tag\":\"PokerCard\"},{\"contents\":["
    "\"Nine\",\"Green\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Jack\","
    "\"Black\"],\"tag\":\"PokerCard\"},{\"contents\":[\"Nine\",\"Red\"],"
    "\"tag\":\"PokerCard\"},{\"contents\":[\"Six\",\"Green\"],\"tag\":"
    "\"PokerCard\"},{\"contents\":[\"Ten\",\"Green\"],\"tag\":\"PokerCard\"},{"
    "\"contents\":[\"Six\",\"Blue\"],\"tag\":\"PokerCard\"}],\"tag\":"
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

int print_tokens(jsmntok_t *current_token, int num_tokens, const char *json,
                 int indent) {

  if (num_tokens <= 0)
    return 0;

  switch (current_token->type) {
  case JSMN_ARRAY: {

    int tokens_printed = 1; // Skip the array one
    if (current_token->size > 0) {

      for (int j = 0; j < indent; ++j)
        printf(" ");
      printf("---- Size %d ----\n", current_token->size);
      for (int i = 0; i < current_token->size; ++i) {
        if (indent == 0)
          printf("-");

        tokens_printed += print_tokens(current_token + tokens_printed,
                                       num_tokens - tokens_printed, json,
                                       indent == 0 ? 1 : indent);
      }
      for (int j = 0; j < indent; ++j)
        printf(" ");
      printf("----\n");
    } else {
      for (int j = 0; j < indent; ++j)
        printf(" ");
      printf("[]\n");
    }
    return tokens_printed;

  } break;
  case JSMN_OBJECT: {
    int tokens_printed = 1; // Skip the object one
    for (int i = 0; i < current_token->size; ++i) {
      jsmntok_t *key = current_token + tokens_printed;
      for (int j = 0; j < indent; ++j)
        printf(" ");
      printf("%.*s:\n", key->end - key->start, json + key->start);
      ++tokens_printed;
      tokens_printed +=
          print_tokens(current_token + tokens_printed,
                       num_tokens - tokens_printed, json, indent + 2);
    }
    return tokens_printed;
  } break;
  case JSMN_PRIMITIVE: {
    for (int j = 0; j < indent; ++j)
      printf(" ");
    printf("%.*s\n", current_token->end - current_token->start,
           json + current_token->start);
    return 1;
  } break;
  case JSMN_STRING: {
    for (int j = 0; j < indent; ++j)
      printf(" ");
    printf("%.*s\n", current_token->end - current_token->start,
           json + current_token->start);
    return 1;
  } break;
  case JSMN_UNDEFINED: {
    for (int j = 0; j < indent; ++j)
      printf(" ");
    printf("UNDEFINED (%.*s)\n", current_token->end - current_token->start,
           json + current_token->start);
    return 1;
  } break;
  default: {
    assert(0 && "Unreachable");
  } break;
  }

  return 0;
}

int parse_playing_card(Card *card, jsmntok_t *game_token,
                       const char *game_json) {
  jsmntok_t *current_token = game_token;

  assert(current_token->type == JSMN_OBJECT &&
         "Playing card must be a json object");
  int object_size = current_token->size;
  assert((object_size == 1 || object_size == 2) &&
         "Object must contain tag and/or content");
  ++current_token;

  for (int o = 0; o < object_size; ++o) {
    jsmntok_t *key = current_token;
    ++current_token;

    unsigned long parsed_keys_mask = 0;
    unsigned long parsed_keys_mask_cmp = 0;
    if (json_str_equal(game_json, key, "contents")) {
      parsed_keys_mask |= 1 << 0;
      assert(current_token->type == JSMN_ARRAY &&
             "Card content must be an array");
      int array_size = current_token->size;
      assert(array_size == 2 && "Card content array size must be 2");
      ++current_token;

      assert(current_token->type == JSMN_STRING &&
             "First card content must be a string");
      if (json_str_equal(game_json, current_token, "Two")) {
        card->number = 2;
      } else if (json_str_equal(game_json, current_token, "Three")) {
        card->number = 3;
      } else if (json_str_equal(game_json, current_token, "Four")) {
        card->number = 4;
      } else if (json_str_equal(game_json, current_token, "Five")) {
        card->number = 5;
      } else if (json_str_equal(game_json, current_token, "Six")) {
        card->number = 6;
      } else if (json_str_equal(game_json, current_token, "Seven")) {
        card->number = 7;
      } else if (json_str_equal(game_json, current_token, "Eight")) {
        card->number = 8;
      } else if (json_str_equal(game_json, current_token, "Nine")) {
        card->number = 9;
      } else if (json_str_equal(game_json, current_token, "Ten")) {
        card->number = 10;
      } else if (json_str_equal(game_json, current_token, "Jack")) {
        card->number = 11;
      } else if (json_str_equal(game_json, current_token, "Queen")) {
        card->number = 12;
      } else if (json_str_equal(game_json, current_token, "King")) {
        card->number = 13;
      } else if (json_str_equal(game_json, current_token, "Ace")) {
        card->number = 14;
      } else {
        fprintf(stderr, "Unknown card value: %.*s\n",
                current_token->end - current_token->start,
                game_json + current_token->start);
        assert(0);
      }
      ++current_token;

      assert(current_token->type == JSMN_STRING &&
             "Second card content must be a string");
      if (json_str_equal(game_json, current_token, "Red")) {
        card->color = RED_CARD;
      } else if (json_str_equal(game_json, current_token, "Green")) {
        card->color = GREEN_CARD;
      } else if (json_str_equal(game_json, current_token, "Blue")) {
        card->color = BLUE_CARD;
      } else if (json_str_equal(game_json, current_token, "Black")) {
        card->color = BLACK_CARD;
      } else {
        fprintf(stderr, "Unknown card color: %.*s\n",
                current_token->end - current_token->start,
                game_json + current_token->start);
        assert(0);
      }
      ++current_token;

    } else if (json_str_equal(game_json, key, "tag")) {
      // data TichuCard = PokerCard (Value, Color) | Dragon | Phoenix | Mahjong
      // | Dog
      parsed_keys_mask |= 1 << 1;
      assert(current_token->type == JSMN_STRING &&
             "The card tag must be a string");
      if (json_str_equal(game_json, current_token, "PokerCard")) {
        // IGNORE
      } else if (json_str_equal(game_json, current_token, "Dragon")) {
        card->color = DRAGON;
      } else if (json_str_equal(game_json, current_token, "Phoenix")) {
        card->color = PHOENIX;
      } else if (json_str_equal(game_json, current_token, "Mahjong")) {
        card->color = MAHJONG;
      } else if (json_str_equal(game_json, current_token, "Dog")) {
        card->color = DOG;
      } else {
        fprintf(stderr, "Unknown card color: %.*s\n",
                current_token->end - current_token->start,
                game_json + current_token->start);
        assert(0);
      }
      ++current_token;
    } else {
      fprintf(stderr, "Unknown key: %.*s\n", key->end - key->start,
              game_json + key->start);
      assert(0);
    }
    parsed_keys_mask_cmp |= 1 << o;
    assert((parsed_keys_mask == parsed_keys_mask_cmp ||
            parsed_keys_mask == 1 << 1) &&
           "Some keys have not been parsed.");
  }

  return current_token - game_token;
}

int parse_tichu_combination(TichuCombination *tichu_combination,
                            jsmntok_t *game_token, const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_OBJECT &&
         "The tichu combination must be an object.");
  (void)tichu_combination;
  (void)game_json;
  assert(0 && "Not yet implemented.");
  return current_token - game_token;
}

int parse_board(Game *game, jsmntok_t *game_token, const char *game_json) {

  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_ARRAY && "Game board must be an array");
  int array_size = current_token->size;
  assert((unsigned long)array_size <= LENGTH(game->board) &&
         "Too many combinations for the board array.");
  ++current_token;
  for (int a = 0; a < array_size; ++a) {
    current_token +=
        parse_tichu_combination(&game->board[a], current_token, game_json);
  }
  return current_token - game_token;
}
int parse_finish_order(Game *game, jsmntok_t *game_token,
                       const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_ARRAY && "Finish order must be an array.");
  int array_size = current_token->size;
  assert((unsigned long)array_size <= LENGTH(game->finishOrder) &&
         "Finish order too long.");
  ++current_token;
  for (int a = 0; a < array_size; ++a) {
    assert(current_token->type == JSMN_STRING &&
           "Finish order must be a string (player name).");
    SAFECPY(game->finishOrder[a], game_json + current_token->start,
            current_token->end - current_token->start);
    ++current_token;
  }
  return current_token - game_token;
}

int str_to_int(const char *str) {
  errno = 0;
  char *endptr;
  int res = strtol(str, &endptr, 10);
  if (errno != 0) {
    perror("strtol");
    assert(0);
  }
  if (endptr == str) {
    fprintf(stderr, "No digits were found\n");
    assert(0);
  }
  return res;
}

int parse_game_phase_tag(Game *game, jsmntok_t *game_token,
                         const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_STRING &&
         "Game phase tag must be a string.");
  if (json_str_equal(game_json, current_token, "Starting")) {
    game->gamePhase.type = Starting;
  } else if (json_str_equal(game_json, current_token, "Dealing")) {
    game->gamePhase.type = Dealing;
  } else if (json_str_equal(game_json, current_token, "Distributing")) {
    game->gamePhase.type = Distributing;
  } else if (json_str_equal(game_json, current_token, "Playing")) {
    game->gamePhase.type = Playing;
  } else if (json_str_equal(game_json, current_token,
                            "GiveAwayLooserTricksAndHands")) {
    game->gamePhase.type = GiveAwayLooserTricksAndHands;
  } else if (json_str_equal(game_json, current_token, "Scoring")) {
    game->gamePhase.type = Scoring;
  } else if (json_str_equal(game_json, current_token, "NextRound")) {
    game->gamePhase.type = NextRound;
  } else if (json_str_equal(game_json, current_token, "Finished")) {
    game->gamePhase.type = Finished;
  } else {
    fprintf(stderr, "Unknown game phose: %.*s\n",
            current_token->end - current_token->start,
            game_json + current_token->start);
    assert(0);
  }
  ++current_token;
  return current_token - game_token;
}

int parse_game_phase_content(Game *game, jsmntok_t *game_token,
                             const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_ARRAY &&
         "Game phase content must be an array.");
  int array_size = current_token->size;
  ++current_token;
  if (array_size == 2 && current_token->type == JSMN_STRING &&
      (current_token + 1)->type == JSMN_PRIMITIVE) {
    // The Playing Phase
    SAFECPY(game->gamePhase.playerName, game_json + current_token->start,
            current_token->end - current_token->start);
    ++current_token;
    game->gamePhase.numPasses = str_to_int(game_json + current_token->start);
    ++current_token;
  } else if (current_token->type == JSMN_OBJECT) {
    // The Dealing Phase
    assert(array_size == LENGTH(game->gamePhase.cards) && "To many or too little cards");
    for (int a = 0; a < array_size; ++a) {
      Card card = {0};
      current_token += parse_playing_card(&card, current_token, game_json);
      game->gamePhase.cards[a] = card;
    }
  } else {
    fprintf(stderr, "Unknown type for game phase content: %d\n",
            current_token->type);
    assert(0);
  }
  return current_token - game_token;
}

int parse_game_phase(Game *game, jsmntok_t *game_token, const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_OBJECT && "Game Phase must be an object.");
  int object_size = current_token->size;
  assert((object_size == 1 || object_size == 2) &&
         "It must contain tag and/or content.");
  ++current_token;

  if (object_size == 1) {
    assert(json_str_equal(game_json, current_token, "tag") &&
           "If only one key it must be tag.");
    ++current_token;
    current_token += parse_game_phase_tag(game, current_token, game_json);
  } else {
    for (int o = 0; o < object_size; ++o) {
      jsmntok_t *key = current_token;
      ++current_token;

      unsigned long parsed_keys_mask = 0;
      unsigned long parsed_keys_mask_cmp = 0;
      if (json_str_equal(game_json, key, "contents")) {
        parsed_keys_mask |= 1 << 0;
        current_token +=
            parse_game_phase_content(game, current_token, game_json);
      } else if (json_str_equal(game_json, key, "tag")) {
        parsed_keys_mask |= 1 << 1;
        current_token += parse_game_phase_tag(game, current_token, game_json);
      } else {
        fprintf(stderr, "Unknown key: %.*s\n", key->end - key->start,
                game_json + key->start);
        assert(0);
      }
      parsed_keys_mask_cmp |= 1 << o;
      assert(parsed_keys_mask == parsed_keys_mask_cmp &&
             "Some keys have not been parsed.");
    }
  }
  return current_token - game_token;
}

int parse_game_config(Game *game, jsmntok_t *game_token,
                      const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_OBJECT &&
         "Game config must be an object.");
  int object_size = current_token->size;
  ++current_token;

  unsigned long parsed_keys_mask = 0;
  unsigned long parsed_keys_mask_cmp = 0;
  jsmntok_t *key = NULL;
  for (int o = 0; o < object_size; ++o) {
    key = current_token;

    ++current_token;
    if (json_str_equal(game_json, key, "scoreLimit")) {
      parsed_keys_mask |= 1 << 0;
      assert(current_token->type == JSMN_PRIMITIVE &&
             "Score Limit must be a primitive.");

      game->gameConfig.scoreLimit =
          str_to_int(game_json + current_token->start);
      ++current_token;
    } else if (json_str_equal(game_json, key, "sittingOrder")) {
      parsed_keys_mask |= 1 << 1;
      assert(current_token->type == JSMN_ARRAY &&
             "Sitting order must be an array.");
      int array_size = current_token->size;
      ++current_token;
      assert(LENGTH(game->gameConfig.sittingOrder) == array_size &&
             "Too many or too little elements for the sitting order.");
      for (int a = 0; a < array_size; ++a) {
        assert(current_token->type == JSMN_STRING &&
               "Element of sitting order must be a string.");
        SAFECPY(game->gameConfig.sittingOrder[a],
                game_json + current_token->start,
                current_token->end - current_token->start);
        ++current_token;
      }

    } else if (json_str_equal(game_json, key, "teamNames")) {
      parsed_keys_mask |= 1 << 2;
      assert(current_token->type == JSMN_ARRAY &&
             "Team names must be an array.");
      int array_size = current_token->size;
      ++current_token;
      assert(LENGTH(game->gameConfig.teamNames) == array_size &&
             "Too many or too little elements for the team names.");
      for (int a = 0; a < array_size; ++a) {
        assert(current_token->type == JSMN_STRING &&
               "Element of team names must be a string.");
        SAFECPY(game->gameConfig.teamNames[a], game_json + current_token->start,
                current_token->end - current_token->start);
        ++current_token;
      }
    } else {
      fprintf(stderr, "Unknown key: %.*s\n", key->end - key->start,
              game_json + key->start);
      assert(0);
    }

    parsed_keys_mask_cmp |= 1 << o;
  }
  assert(parsed_keys_mask == parsed_keys_mask_cmp &&
         "Some keys have not been parsed.");
  return current_token - game_token;
}

int get_sitting_order_index(
    PlayerName sitting_order[NUM_PLAYERS][MAX_BYTES_NAME],
    const PlayerName *player_name, int str_len) {
  for (int i = 0; i < NUM_PLAYERS; ++i)
    if (strncmp(sitting_order[i], player_name, str_len) == 0)
      return i;

  assert(0 && "Player not found.");
}

int get_team_name_index(TeamName team_names[NUM_TEAMS][MAX_BYTES_NAME],
                        const TeamName *team_name, int str_len) {
  for (int i = 0; i < NUM_TEAMS; ++i)
    if (strncmp(team_names[i], team_name, str_len) == 0)
      return i;

  assert(0 && "Team name not found.");
}

int parse_hands(Game *game, jsmntok_t *game_token, const char *game_json,
                PlayerName (*sitting_order)[NUM_PLAYERS][MAX_BYTES_NAME]) {
  jsmntok_t *current_token = game_token;
  assert(sitting_order != NULL && "Game config must be parsed at this point.");
  assert(current_token->type == JSMN_OBJECT && "The hands must be an object");
  int object_size = current_token->size;
  assert(object_size == NUM_PLAYERS &&
         "The there must be keys for each player.");
  ++current_token;
  unsigned long parsed_keys_mask = 0;
  unsigned long parsed_keys_mask_cmp = 0;
  for (int o = 0; o < object_size; ++o) {
    jsmntok_t *key = current_token;
    assert(key->type == JSMN_STRING && "The key must be a string.");
    int idx = get_sitting_order_index(*sitting_order, game_json + key->start,
                                      key->end - key->start);
    parsed_keys_mask |= 1 << idx;
    parsed_keys_mask_cmp |= 1 << o;
    ++current_token;

    assert(current_token->type == JSMN_ARRAY &&
           "The hands must contain an array");
    int array_size = current_token->size;
    assert((unsigned long)array_size < LENGTH(game->hands[0]) &&
           "Array size must be smaller the hands array.");
    ++current_token;
    for (int a = 0; a < array_size; ++a) {
      current_token +=
          parse_playing_card(&game->hands[idx][a], current_token, game_json);
    }
  }
  assert(parsed_keys_mask_cmp == parsed_keys_mask &&
         "Some players hands have not been parsed.");
  return current_token - game_token;
}

int parse_scores(Game *game, jsmntok_t *game_token, const char *game_json,
                 PlayerName (*team_names)[NUM_TEAMS][MAX_BYTES_NAME]) {
  jsmntok_t *current_token = game_token;
  assert(team_names != NULL && "Game config must be parsed at this point.");
  assert(current_token->type == JSMN_OBJECT && "The scores must be an object");
  int object_size = current_token->size;
  assert(object_size == NUM_TEAMS && "The there must be keys for each team.");
  ++current_token;
  unsigned long parsed_keys_mask = 0;
  unsigned long parsed_keys_mask_cmp = 0;
  for (int o = 0; o < object_size; ++o) {
    jsmntok_t *key = current_token;
    assert(key->type == JSMN_STRING && "The key must be a string.");
    int idx = get_team_name_index(*team_names, game_json + key->start,
                                  key->end - key->start);
    parsed_keys_mask |= 1 << idx;
    parsed_keys_mask_cmp |= 1 << o;
    ++current_token;

    assert(current_token->type == JSMN_PRIMITIVE &&
           "The score must be a number.");
    game->scores[idx] = str_to_int(game_json + current_token->start);
    ++current_token;
  }
  assert(parsed_keys_mask_cmp == parsed_keys_mask &&
         "Some scores have not been parsed.");
  return current_token - game_token;
}

int parse_tichus(Game *game, jsmntok_t *game_token, const char *game_json,
                 PlayerName (*sitting_order)[NUM_PLAYERS][MAX_BYTES_NAME]) {
  jsmntok_t *current_token = game_token;
  assert(sitting_order != NULL && "Game config must be parsed at this point.");
  assert(current_token->type == JSMN_OBJECT && "The tichus must be an object");
  int object_size = current_token->size;
  assert(object_size == NUM_PLAYERS &&
         "The there must be keys for each player.");
  ++current_token;
  unsigned long parsed_keys_mask = 0;
  unsigned long parsed_keys_mask_cmp = 0;
  for (int o = 0; o < object_size; ++o) {
    jsmntok_t *key = current_token;
    assert(key->type == JSMN_STRING && "The key must be a string.");
    int idx = get_sitting_order_index(*sitting_order, game_json + key->start,
                                      key->end - key->start);
    parsed_keys_mask |= 1 << idx;
    parsed_keys_mask_cmp |= 1 << o;
    ++current_token;

    if (current_token->type == JSMN_PRIMITIVE) {
      assert(*(game_json + current_token->start) == 'n' &&
             "If it is a primitive it must be null.");
      game->tichus[idx] = NoTichu;
      ++current_token;

    } else if (current_token->type == JSMN_OBJECT) {
      assert(current_token->size == 1 && "There must only be one tichu type.");
      ++current_token;
      assert(json_str_equal(game_json, current_token, "tag") &&
             "The key must be called tag.");
      ++current_token;
      if (json_str_equal(game_json, current_token, "Tichu")) {
        game->tichus[idx] = Tichu;
      } else if (json_str_equal(game_json, current_token, "GrandTichu")) {
        game->tichus[idx] = GrandTichu;
      } else {
        fprintf(stderr, "Unknown Tichu Type: %.*s\n",
                current_token->end - current_token->start,
                game_json + current_token->start);
        assert(0);
      }
      ++current_token;
    } else {
      fprintf(stderr, "Unknown Json Type %d for %.*s\n", current_token->type,
              current_token->end - current_token->start,
              game_json + current_token->start);
      assert(0);
    }
  }
  assert(parsed_keys_mask_cmp == parsed_keys_mask &&
         "Some players tichus have not been parsed.");
  return current_token - game_token;
}

int parse_tricks(Game *game, jsmntok_t *game_token, const char *game_json,
                 PlayerName (*sitting_order)[NUM_PLAYERS][MAX_BYTES_NAME]) {
  jsmntok_t *current_token = game_token;
  assert(sitting_order != NULL && "Game config must be parsed at this point.");
  assert(current_token->type == JSMN_OBJECT && "The tricks must be an object");
  int object_size = current_token->size;
  assert(object_size == NUM_PLAYERS &&
         "The there must be keys for each player.");
  ++current_token;
  unsigned long parsed_keys_mask = 0;
  unsigned long parsed_keys_mask_cmp = 0;
  for (int o = 0; o < object_size; ++o) {
    jsmntok_t *key = current_token;
    assert(key->type == JSMN_STRING && "The key must be a string.");
    int idx = get_sitting_order_index(*sitting_order, game_json + key->start,
                                      key->end - key->start);
    parsed_keys_mask |= 1 << idx;
    parsed_keys_mask_cmp |= 1 << o;
    ++current_token;

    assert(current_token->type == JSMN_ARRAY &&
           "The tricks must contain an array");
    int array_size = current_token->size;
    assert((unsigned long)array_size < LENGTH(game->tricks[0]) &&
           "Array size must be smaller the tricks array.");
    ++current_token;
    for (int a = 0; a < array_size; ++a) {
      current_token +=
          parse_playing_card(&game->tricks[idx][a], current_token, game_json);
    }
  }
  assert(parsed_keys_mask_cmp == parsed_keys_mask &&
         "Some players tricks have not been parsed.");
  return current_token - game_token;
}

int parse_game(Game *game, jsmntok_t *game_token, const char *game_json) {
  memset(game, 0, sizeof(*game));
  jsmntok_t *current_token = game_token;

  assert(current_token->type == JSMN_OBJECT &&
         "Element must be the game struct.");
  int object_size = current_token->size;
  ++current_token;

  unsigned long parsed_keys_mask = 0;
  unsigned long parsed_keys_mask_cmp = 0;
  jsmntok_t *key = NULL;
  PlayerName(*sitting_order)[NUM_PLAYERS][MAX_BYTES_NAME] = NULL;
  TeamName(*team_names)[NUM_TEAMS][MAX_BYTES_NAME] = NULL;
  for (int o = 0; o < object_size; ++o) {
    key = current_token;
    ++current_token;
    if (json_str_equal(game_json, key, "board")) {
      parsed_keys_mask |= 1 << 0;
      current_token += parse_board(game, current_token, game_json);

    } else if (json_str_equal(game_json, key, "currentDealer")) {
      parsed_keys_mask |= 1 << 1;
      assert(current_token->type == JSMN_STRING &&
             "Current dealer must be a string.");
      SAFECPY(game->currentDealer, game_json + current_token->start,
              current_token->end - current_token->start);
      ++current_token;
    } else if (json_str_equal(game_json, key, "finishOrder")) {
      parsed_keys_mask |= 1 << 2;
      current_token += parse_finish_order(game, current_token, game_json);
    } else if (json_str_equal(game_json, key, "gameConfig")) {
      parsed_keys_mask |= 1 << 3;
      current_token += parse_game_config(game, current_token, game_json);
      sitting_order = &game->gameConfig.sittingOrder;
      team_names = &game->gameConfig.teamNames;
    } else if (json_str_equal(game_json, key, "gamePhase")) {
      parsed_keys_mask |= 1 << 4;
      current_token += parse_game_phase(game, current_token, game_json);
    } else if (json_str_equal(game_json, key, "generator")) {
      parsed_keys_mask |= 1 << 5;
      assert(current_token->type == JSMN_ARRAY && current_token->size == 2 &&
             "Generator must be an array of size 2");
      ++current_token;
      assert(current_token->type == JSMN_PRIMITIVE &&
             "First generator must be a number");
      ++current_token;
      assert(current_token->type == JSMN_PRIMITIVE &&
             "Second generator must be a number");
      ++current_token;

      // NOTE: Don't save it at the moment.
    } else if (json_str_equal(game_json, key, "hands")) {
      parsed_keys_mask |= 1 << 6;
      current_token +=
          parse_hands(game, current_token, game_json, sitting_order);
    } else if (json_str_equal(game_json, key, "scores")) {
      parsed_keys_mask |= 1 << 7;
      current_token += parse_scores(game, current_token, game_json, team_names);
    } else if (json_str_equal(game_json, key, "shouldGameStop")) {
      parsed_keys_mask |= 1 << 8;
      game->shouldGameStop = json_bool(game_json, current_token);
      ++current_token;
    } else if (json_str_equal(game_json, key, "tichus")) {
      parsed_keys_mask |= 1 << 9;
      current_token +=
          parse_tichus(game, current_token, game_json, sitting_order);

    } else if (json_str_equal(game_json, key, "tricks")) {
      parsed_keys_mask |= 1 << 10;
      current_token +=
          parse_tricks(game, current_token, game_json, sitting_order);
    } else if (json_str_equal(game_json, key, "winnerTeams")) {
      parsed_keys_mask |= 1 << 11;
      assert(current_token->type == JSMN_ARRAY &&
             "The winner teams must be an array");
      int array_size = current_token->size;
      assert((unsigned long)array_size <= LENGTH(game->winnerTeams) &&
             "There must be less or equal winner teams than amount of teams");
      ++current_token;
      for (int a = 0; a < array_size; ++a) {
        assert(current_token->type == JSMN_STRING &&
               "The winner team must be a string.");
        SAFECPY(game->winnerTeams[a], game_json + current_token->start,
                current_token->end - current_token->start);
        ++current_token;
      }

    } else {
      fprintf(stderr, "Unknown key: %.*s\n", key->end - key->start,
              game_json + key->start);
      assert(0);
    }
    parsed_keys_mask_cmp |= 1 << o;
  }
  assert(parsed_keys_mask == parsed_keys_mask_cmp &&
         "Some keys have not been parsed.");

  return current_token - game_token;
}

void parse_game_and_actions(Game *game, const char *game_json) {
  (void)game;

  int num_tokens = jsmn_parse(&json_parser, game_json, strlen(game_json),
                              json_tokens, NUM_JSON_TOKENS);
  jsmntok_t *current_token = &json_tokens[0];
  if (num_tokens < 0) {
    print_json_error(num_tokens);
    assert(0);
  }
  print_tokens(json_tokens, num_tokens, game_json, 0);
  assert(num_tokens > 0 && "Not enough tokens.");
  assert(current_token->type == JSMN_ARRAY &&
         "First element must be an array.");
  assert(current_token->size == 3 &&
         "There must be 2 elements in the array. There is a bug in the json "
         "library so it shows one more if the root element is an array.");
  ++current_token;
  current_token += parse_game(game, current_token, game_json);
}

#if !WASM
int main(void) {
  /* init(); */

  Game game = {0};
  parse_game_and_actions(&game, test_json);

  /* while (!WindowShouldClose()) { */
  /*   update_draw(); */
  /* } */
  /**/
  /* deinit(); */
}
#endif
