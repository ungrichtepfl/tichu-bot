#include "game.h"
#include "parser.h"
#include "test_json.h"
#include <raylib.h>

#if defined(__EMSCRIPTEN__) || defined(__wasm__) || defined(__wasm32__) ||     \
    defined(__wasm64__)
#define WASM 1
#else
#define WASM 0
#endif

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

#define ACTION_CPY(s)                                                          \
  do {                                                                         \
    assert(sizeof(g_game_state.current_action) >= strlen(s) &&                 \
           "Action is too long");                                              \
    strcpy(g_game_state.current_action, s);                                    \
  } while (0)
#define ACTION_RESET() ACTION_CPY("null")

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
} RenderState;

RenderState g_render_state = {0};

GameState g_game_state = {0};
static_assert(LENGTH(g_game_state.player_actions) ==
                  LENGTH(g_game_state.num_actions),
              "Must be the same length");

void set_highest_prio(size_t idx) {
  int cut = -1;
  for (int i = 0; (size_t)i < LENGTH(g_render_state.render_prio); ++i) {
    if (g_render_state.render_prio[i] == idx) {
      cut = i;
      break;
    }
  }
  if (cut == -1) {
    fprintf(stderr, "Could not set highest prio, idx not found. Most probably "
                    "something wrong in the init function.");
    return;
  }
  memmove(&g_render_state.render_prio[1], &g_render_state.render_prio[0],
          cut * sizeof(g_render_state.render_prio[0]));
  g_render_state.render_prio[0] = idx;
}

size_t get_card_index(Card card) {
  if (card.color < NUM_COLORS)
    return card.color * CARDS_PER_COLOR + card.number - 2;
  else
    return card.color - NUM_COLORS + CARDS_PER_COLOR * NUM_COLORS;
}

CardPose get_card_pose(Card card) {
  return g_render_state.card_pose[get_card_index(card)];
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
    if (g_render_state.selected_piece_idx >= 0) {
      g_render_state.card_pose[g_render_state.selected_piece_idx].pos.x +=
          mouse_touch.x - previous_mouse_touch.x;
      g_render_state.card_pose[g_render_state.selected_piece_idx].pos.y +=
          mouse_touch.y - previous_mouse_touch.y;
    } else {
      for (size_t i = 0; i < LENGTH(g_render_state.render_prio); ++i) {
        size_t idx = g_render_state.render_prio[i];
        Card card = get_card_from_index(idx);
        Rectangle card_rec = get_card_rectangle(card);
        if (CheckCollisionPointRec(mouse_touch, card_rec)) {
          g_render_state.selected_piece_idx = idx;
          set_highest_prio(idx);
          break;
        }
      }
    }
    previous_mouse_touch = mouse_touch;
  } else {
    g_render_state.selected_piece_idx = -1;
  }
}

void setup_global_game_state(void) {
  g_render_state.selected_piece_idx = -1; // Nothing selected

  for (size_t i = 0; i < LENGTH(g_render_state.render_prio); ++i) {
    g_render_state.render_prio[i] = i;
  }

  for (size_t i = 0; i < LENGTH(g_render_state.card_pose); ++i) {
    int col = NUM_COLORS + 1;
    // No scale
    g_render_state.card_pose[i].scale = 1.f;
    // No rotation
    g_render_state.card_pose[i].rot = 0.f;

    // Evenly space the cards
    g_render_state.card_pose[i].pos.x = (i % col) * ((float)WIN_WIDTH / col);
    g_render_state.card_pose[i].pos.y =
        ((float)i / col) * ((float)WIN_HEIHT / col);
    // Set the last 4 cards to be in the middle
  }
  ACTION_RESET();
}

void draw_cards(void) {
  for (int i = (int)LENGTH(g_render_state.render_prio) - 1; i >= 0; --i) {
    size_t idx = g_render_state.render_prio[i];
    Card card = get_card_from_index(idx);
    CardPose pose = g_render_state.card_pose[idx];
    Texture2D card_texture = get_card_asset(card);
    DrawTextureEx(card_texture, pose.pos, pose.rot, pose.scale, WHITE);
  }
}

void init(void) {
  setup_global_game_state();
  InitWindow(WIN_WIDTH, WIN_HEIHT, "Tichu");
  load_global_assets();
#if !WASM
  SetTargetFPS(FPS);
#endif
}

void deinit(void) {
  for (unsigned long i = 0; i < LENGTH(g_game_state.player_actions); ++i)
    free(g_game_state.player_actions[i]);
  unload_global_assets();
  CloseWindow();
}

void update_draw(const char *game_json) {

  parse_game_and_actions(&g_game_state, game_json);

  update_card_position();

  BeginDrawing();
  ClearBackground(WHITE);
  DrawTexture(get_background_asset(), 0, 0, WHITE);
  draw_cards();

  EndDrawing();
}

const char *get_current_action(void) { return g_game_state.current_action; }

#if !WASM && !HASKELL
int main(void) {
  init();

  parse_game_and_actions(&g_game_state, test_json1);
  parse_game_and_actions(&g_game_state, test_json2);

  while (!WindowShouldClose()) {
    update_draw(test_json1);
  }

  deinit();
}
#endif
