#include "game.h"
#include "parser.h"
#include "test_json.h"
#include <limits.h>
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

#define FONT_SIZE_BIG 50.f
#define CHAR_SIZE_BIG 42.f
#define FONT_SIZE_MEDIUM 40.f
#define CHAR_SIZE_MEDIUM 33.f
#define FONT_SIZE_SMALL 30.f
#define CHAR_SIZE_SMALL 25.f

#define BUFFCPY(b, s) strncpy(b, s, sizeof(b) - 1);

#define ACTION_CPY(s)                                                          \
  do {                                                                         \
    assert(sizeof(g_game_state.current_action_json) > strlen(s) &&             \
           "Action is too long");                                              \
    strcpy(g_game_state.current_action_json, s);                               \
  } while (0)
#define ACTION_RESET() ACTION_CPY("null")

#define CONFIG_CPY(s)                                                          \
  do {                                                                         \
    assert(sizeof(g_pre_game_state.game_config_json) > strlen(s) &&            \
           "Config is too long");                                              \
    strcpy(g_pre_game_state.game_config_json, s);                              \
  } while (0)
#define CONFIG_RESET() CONFIG_CPY("null")

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

PreGameState g_pre_game_state = {0};
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

#define BOX_CHAR_SIZE CHAR_SIZE_BIG
#define BOX_FONT_SIZE FONT_SIZE_BIG
#define BOX_PADDING ((float)BOX_CHAR_SIZE / 3.5)

#define LABEL_CHAR_SIZE CHAR_SIZE_MEDIUM
#define LABEL_FONT_SIZE FONT_SIZE_MEDIUM
#define LABEL_PADDING_BOX ((float)LABEL_CHAR_SIZE / 2.5)

#define TITLE_CHAR_SIZE CHAR_SIZE_BIG
#define TITLE_FONT_SIZE FONT_SIZE_BIG
#define TITLE_PADDING_BOX (3.f * BOX_CHAR_SIZE)

#define BUTTON_CHAR_SIZE CHAR_SIZE_MEDIUM
#define BUTTON_FONT_SIZE FONT_SIZE_MEDIUM
#define BUTTON_PADDING ((float)BUTTON_CHAR_SIZE / 3.5)
#define BUTTON_PADDING_BOX (1.f * BOX_CHAR_SIZE)
#define BUTTON_WIDTH ((float)WIN_WIDTH / 5.f)
#define BUTTON_HEIGHT ((float)BUTTON_CHAR_SIZE + 2.f * BUTTON_PADDING)

#define ERROR_CHAR_SIZE CHAR_SIZE_SMALL
#define ERROR_FONT_SIZE FONT_SIZE_SMALL
#define ERROR_PADDING ((float)ERROR_CHAR_SIZE / 3.5)

float get_title_y(int number_of_text_boxes, float textbox_height,
                  float textbox_dy) {

  return (float)WIN_HEIHT / 2.f - (TITLE_PADDING_BOX + TITLE_CHAR_SIZE +
                                   number_of_text_boxes * textbox_height +
                                   (number_of_text_boxes - 1) * textbox_dy +
                                   BUTTON_PADDING + BUTTON_HEIGHT) /
                                      2.f;
}

void set_pre_game_state_start(void) {
  int number_of_text_boxes = 0;
  float title_y = get_title_y(number_of_text_boxes, 0, 0);
  int button_x = (float)WIN_WIDTH / 2 - (float)BUTTON_WIDTH / 2;
  int button_y = title_y + TITLE_CHAR_SIZE + BUTTON_PADDING_BOX;

  g_pre_game_state.number_of_text_boxes = number_of_text_boxes;

  BUFFCPY(g_pre_game_state.title, "Welcom to Tichu");
  g_pre_game_state.title_y = title_y;

  g_pre_game_state.button =
      (Rectangle){button_x, button_y, BUTTON_WIDTH, BUTTON_HEIGHT};
  BUFFCPY(g_pre_game_state.button_text, "Play");
}

void set_pre_game_state_team_names(void) {
  int number_of_text_boxes = NUM_TEAMS;
  float height = (float)BOX_CHAR_SIZE + 2.f * BOX_PADDING;
  float dy = (float)WIN_HEIHT / 12;
  float title_y = get_title_y(number_of_text_boxes, height, dy);
  float width = BOX_CHAR_SIZE * MAX_CHARS_NAME;
  float x = (float)WIN_WIDTH / 2 - width / 2;
  float y = title_y + TITLE_PADDING_BOX;

  g_pre_game_state.number_of_text_boxes = number_of_text_boxes;
  g_pre_game_state.selected_text_box = 0;
  BUFFCPY(g_pre_game_state.title, "Choose Team Names");
  g_pre_game_state.title_y = title_y;
  BUFFCPY(g_pre_game_state.text_box_label[0], "Team 1:");
  BUFFCPY(g_pre_game_state.text_box_label[1], "Team 2:");
  g_pre_game_state.text_box[0] = (Rectangle){x, y, width, height};
  g_pre_game_state.text_box[1] = (Rectangle){x, y + height + dy, width, height};
  for (int i = 0; i < number_of_text_boxes; ++i) {
    g_pre_game_state.text_box_input[i][0] = '\0';
    g_pre_game_state.input_char_counter[i] = 0;
  }

  int button_x = (float)WIN_WIDTH / 2 - (float)BUTTON_WIDTH / 2;
  int button_y = g_pre_game_state.text_box[1].y +
                 g_pre_game_state.text_box[1].height + BUTTON_PADDING_BOX;
  g_pre_game_state.button =
      (Rectangle){button_x, button_y, BUTTON_WIDTH, BUTTON_HEIGHT};
  BUFFCPY(g_pre_game_state.button_text, "Next");
}

void set_pre_game_state_score_limit(void) {
  int number_of_text_boxes = 1;
  float height = (float)BOX_CHAR_SIZE + 2.f * BOX_PADDING;
  float dy = (float)WIN_HEIHT / 12;
  float title_y = get_title_y(number_of_text_boxes, height, dy);
  float width = BOX_CHAR_SIZE * MAX_CHARS_NAME;
  float x = (float)WIN_WIDTH / 2 - width / 2;
  float y = title_y + TITLE_PADDING_BOX;

  g_pre_game_state.number_of_text_boxes = number_of_text_boxes;
  g_pre_game_state.selected_text_box = 0;
  BUFFCPY(g_pre_game_state.title, "Choose Score Limit");
  g_pre_game_state.title_y = title_y;
  BUFFCPY(g_pre_game_state.text_box_label[0], "Max Score:");
  g_pre_game_state.text_box[0] = (Rectangle){x, y, width, height};
  BUFFCPY(g_pre_game_state.text_box_input[0], "1000");
  g_pre_game_state.input_char_counter[0] =
      strlen(g_pre_game_state.text_box_input[0]);

  int button_x = (float)WIN_WIDTH / 2 - (float)BUTTON_WIDTH / 2;
  int button_y = g_pre_game_state.text_box[0].y +
                 g_pre_game_state.text_box[0].height + BUTTON_PADDING_BOX;
  g_pre_game_state.button =
      (Rectangle){button_x, button_y, BUTTON_WIDTH, BUTTON_HEIGHT};
  BUFFCPY(g_pre_game_state.button_text, "Next");
}

void set_pre_game_state_player_names(void) {
  int number_of_text_boxes = NUM_PLAYERS;
  float height = (float)BOX_CHAR_SIZE + 2.f * BOX_PADDING;
  float dy = (float)WIN_HEIHT / 12;
  float title_y = get_title_y(number_of_text_boxes, height, dy);
  float width = BOX_CHAR_SIZE * MAX_CHARS_NAME;
  float x = (float)WIN_WIDTH / 2 - width / 2;
  float y = title_y + TITLE_PADDING_BOX;

  g_pre_game_state.number_of_text_boxes = number_of_text_boxes;
  g_pre_game_state.selected_text_box = 0;
  BUFFCPY(g_pre_game_state.title, "Choose Player Names");
  g_pre_game_state.title_y = title_y;
  BUFFCPY(g_pre_game_state.text_box_label[0], "Player 1:");
  BUFFCPY(g_pre_game_state.text_box_label[1], "Player 2:");
  BUFFCPY(g_pre_game_state.text_box_label[2], "Player 3:");
  BUFFCPY(g_pre_game_state.text_box_label[3], "Player 4:");
  g_pre_game_state.text_box[0] = (Rectangle){x, y, width, height};
  g_pre_game_state.text_box[1] = (Rectangle){x, y + height + dy, width, height};
  g_pre_game_state.text_box[2] =
      (Rectangle){x, y + 2 * (height + dy), width, height};
  g_pre_game_state.text_box[3] =
      (Rectangle){x, y + 3 * (height + dy), width, height};
  for (int i = 0; i < number_of_text_boxes; ++i) {
    g_pre_game_state.text_box_input[i][0] = '\0';
    g_pre_game_state.input_char_counter[i] = 0;
  }

  int button_x = (float)WIN_WIDTH / 2 - (float)BUTTON_WIDTH / 2;
  int button_y = g_pre_game_state.text_box[3].y +
                 g_pre_game_state.text_box[3].height + BUTTON_PADDING_BOX;
  g_pre_game_state.button =
      (Rectangle){button_x, button_y, BUTTON_WIDTH, BUTTON_HEIGHT};
  BUFFCPY(g_pre_game_state.button_text, "Next");
}

void set_new_pre_game_state_phase() {
  char *error = NULL;
  switch (g_pre_game_state.phase) {
  case PGS_START: {
    set_pre_game_state_start();
  } break;
  case PGS_PLAYER_NAMES: {
    set_pre_game_state_player_names();
  } break;
  case PGS_TEAM_NAMES: {
    for (int i = 0; i < NUM_PLAYERS; ++i) {
      if (strlen(g_pre_game_state.text_box_input[i]) == 0) {
        error = "No empty teamname allowed.";
        goto error;
      }
      BUFFCPY(g_pre_game_state.game_config.sitting_order[i],
              g_pre_game_state.text_box_input[i]);
    }
    set_pre_game_state_team_names();
  } break;
  case PGS_MAX_SCORE: {
    for (int i = 0; i < NUM_TEAMS; ++i) {
      if (strlen(g_pre_game_state.text_box_input[i]) == 0) {
        error = "No empty teams allowed.";
        goto error;
      }
      BUFFCPY(g_pre_game_state.game_config.team_names[i],
              g_pre_game_state.text_box_input[i]);
    }
    set_pre_game_state_score_limit();
  } break;
  case PGS_FINISHED: {
    if (strlen(g_pre_game_state.text_box_input[0]) == 0) {
      error = "No empty score allowed.";
      goto error;
    }

    errno = 0;
    char *endptr;
    char *str = g_pre_game_state.text_box_input[0];
    long res = strtol(str, &endptr, 10);

    if (errno != 0) {
      error = strerror(errno);
      goto error;
    }
    if (endptr == str) {
      error = "No digits were found";
      goto error;
    }
    if (*endptr != '\0' && *endptr != ' ') {
      error = "The input must be a whole number";
      goto error;
    }
    if (res > INT_MAX) {
      error = "Score limit is too high";
      goto error;
    }
    if (res <= 0) {
      error = "Score limit must be greater than 0";
      goto error;
    }

    g_pre_game_state.game_config.score_limit = (int)res;

    // TODO: SERIALIZE JSON
  } break;
  default:
    assert(0 && "Unreachable.");
  }
  g_pre_game_state.error[0] = '\0'; // No error happened
  return;
error:
  BUFFCPY(g_pre_game_state.error, error);
  --g_pre_game_state.phase;
  return;
}

void reset_global_pre_game_state(void) {

  memset(&g_pre_game_state, 0, sizeof(g_pre_game_state));
  set_new_pre_game_state_phase();
  CONFIG_RESET();
}

void reset_global_game_state(void) {
  memset(&g_render_state, 0, sizeof(g_render_state));
  memset(&g_game_state, 0, sizeof(g_game_state));
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

void reset_game(void) {
  reset_global_pre_game_state();
  reset_global_game_state();
}

void init(void) {
  reset_game();
  InitWindow(WIN_WIDTH, WIN_HEIHT, "Tichu");
  load_global_assets();
  SetTargetFPS(FPS);
}

void deinit(void) {
  for (unsigned long i = 0; i < LENGTH(g_game_state.player_actions); ++i)
    free(g_game_state.player_actions[i]);
  unload_global_assets();
  CloseWindow();
}

const char *update_draw_config(void) {
  // Update
  //----------------------------------------------------------------------------------
  if (g_pre_game_state.phase == PGS_FINISHED) {
    assert(0 && "Should not happen!");
  }

  if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT) &&
      CheckCollisionPointRec(GetMousePosition(), g_pre_game_state.button)) {
    ++g_pre_game_state.phase;
    set_new_pre_game_state_phase();
  }

  // Check if a new textbox is selected
  if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
    for (unsigned long i = 0; i < g_pre_game_state.number_of_text_boxes; ++i) {
      if (CheckCollisionPointRec(GetMousePosition(),
                                 g_pre_game_state.text_box[i])) {
        g_pre_game_state.selected_text_box = i;
        g_pre_game_state.frame_counter = 0; // Trigger draw of blinking cursor
        break;
      }
    }
  }

  // Check if the cursor is over a textbox
  SetMouseCursor(MOUSE_CURSOR_DEFAULT);
  for (unsigned long i = 0; i < g_pre_game_state.number_of_text_boxes; ++i) {
    if (CheckCollisionPointRec(GetMousePosition(),
                               g_pre_game_state.text_box[i])) {
      SetMouseCursor(MOUSE_CURSOR_IBEAM);
      break;
    }
  }

  // Currently selected textbox and input
  Rectangle *selected_tb =
      &g_pre_game_state.text_box[g_pre_game_state.selected_text_box];
  char (*selected_tb_input)[MAX_CHARS_NAME] =
      &g_pre_game_state.text_box_input[g_pre_game_state.selected_text_box];
  int *selected_char_counter =
      &g_pre_game_state.input_char_counter[g_pre_game_state.selected_text_box];

  int key = GetCharPressed();
  while (key > 0) {
    // NOTE: Only allow keys in range [32..125]
    if ((*selected_char_counter < MAX_CHARS_NAME) && (key >= 32) &&
        (key <= 125)) {
      (*selected_tb_input)[*selected_char_counter] = (char)key;
      (*selected_tb_input)[*selected_char_counter + 1] = '\0';
      (*selected_char_counter)++;
    }
    key = GetCharPressed(); // Check next character in the queue
  }

  if (IsKeyPressedRepeat(KEY_BACKSPACE) || IsKeyPressed(KEY_BACKSPACE)) {
    (*selected_char_counter)--;
    if (*selected_char_counter < 0)
      *selected_char_counter = 0;

    (*selected_tb_input)[*selected_char_counter] = '\0';
  }

  ++g_pre_game_state.frame_counter;
  //----------------------------------------------------------------------------------

  // Draw
  //----------------------------------------------------------------------------------

  BeginDrawing();

  DrawTexture(get_background_asset(), 0, 0, WHITE);

  // Title
  DrawText(g_pre_game_state.title,
           (float)WIN_WIDTH / 2.f -
               MeasureText(g_pre_game_state.title, TITLE_FONT_SIZE) / 2.f,
           g_pre_game_state.title_y, TITLE_FONT_SIZE, BLACK);

  // Draw Textboxes, Labels and Contents
  for (unsigned long i = 0; i < g_pre_game_state.number_of_text_boxes; ++i) {
    // Boxes
    Rectangle text_box = g_pre_game_state.text_box[i];
    DrawRectangleRec(text_box, LIGHTGRAY);
    Color color = g_pre_game_state.selected_text_box == i ? RED : BLACK;
    DrawRectangleLines(text_box.x, text_box.y, text_box.width, text_box.height,
                       color);
    // Text
    DrawText(g_pre_game_state.text_box_input[i],
             g_pre_game_state.text_box[i].x + BOX_PADDING,
             g_pre_game_state.text_box[i].y + BOX_PADDING, BOX_FONT_SIZE,
             BLACK);

    // Label
    DrawText(g_pre_game_state.text_box_label[i],
             g_pre_game_state.text_box[i].x + LABEL_PADDING_BOX,
             g_pre_game_state.text_box[i].y - LABEL_PADDING_BOX -
                 LABEL_CHAR_SIZE,
             LABEL_FONT_SIZE, BLACK);
  }

  // Draw blinking underscore char
  if (g_pre_game_state.number_of_text_boxes > 0 &&
      *selected_char_counter < MAX_CHARS_NAME &&
      (g_pre_game_state.frame_counter / 20) % 2 == 0) {

    DrawText("_",
             selected_tb->x + BOX_PADDING +
                 3.f // To give some space to next char
                 + MeasureText(*selected_tb_input, BOX_FONT_SIZE),
             selected_tb->y + BOX_PADDING, BOX_FONT_SIZE, MAROON);
  }

  // Button
  DrawRectangleRec(g_pre_game_state.button, BLUE);
  DrawRectangleLines(g_pre_game_state.button.x, g_pre_game_state.button.y,
                     g_pre_game_state.button.width,
                     g_pre_game_state.button.height, DARKBLUE);
  DrawText(
      g_pre_game_state.button_text,
      (float)(2 * g_pre_game_state.button.x + g_pre_game_state.button.width) /
              2.f -
          (float)MeasureText(g_pre_game_state.button_text, BUTTON_FONT_SIZE) /
              2.f,
      g_pre_game_state.button.y + BUTTON_PADDING, BUTTON_FONT_SIZE, BLACK);

  // ERROR
  DrawText(
      g_pre_game_state.error,
      WIN_WIDTH / 2.f -
          MeasureText(g_pre_game_state.error, ERROR_FONT_SIZE) / 2.f,
      (g_pre_game_state.button.y + g_pre_game_state.button.height + WIN_HEIHT) /
              2.f -
          ERROR_CHAR_SIZE / 2.f,
      ERROR_FONT_SIZE, RED);

  EndDrawing();
  //----------------------------------------------------------------------------------
  return g_pre_game_state.game_config_json;
}

const char *update_draw_game(const char *game_json) {

  parse_game_and_actions(&g_game_state, game_json);

  update_card_position();

  BeginDrawing();
  ClearBackground(WHITE);
  DrawTexture(get_background_asset(), 0, 0, WHITE);
  draw_cards();

  EndDrawing();

  return g_game_state.current_action_json;
}

bool window_should_close(void) { return WindowShouldClose(); }
