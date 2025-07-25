#include "game.h"
#include "parser.h"
#include <limits.h>
#include <raylib.h>
#include <stddef.h>

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
#define ASSET_BACK "back.png"
#define ASSET_BACK_ROTATED "back_rotated.png"
#define ASSET_BACKGROUND "background.png"

#define ASSET_PATH "./gui/images/"
#define CARD_ASSET_REL_PATH "cards/"

#define FONT_SIZE_BIG 50.f
#define CHAR_SIZE_BIG 42.f
#define FONT_SIZE_MEDIUM 40.f
#define CHAR_SIZE_MEDIUM 33.f
#define FONT_SIZE_SMALL 30.f
#define CHAR_SIZE_SMALL 25.f

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
  Texture2D back;
  Texture2D back_rotated;
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
  bool visible[TOTAL_CARDS];
  bool selectable[TOTAL_CARDS];
  bool show_front[TOTAL_CARDS];
  bool rotated_back[TOTAL_CARDS];
  bool selected[TOTAL_CARDS];
  int selected_card_idx_mouse;
  Rectangle playing_field;
  Rectangle play_button;
  Rectangle tichu_button;
  float bottom_player_label_y;
  float top_player_label_y;
  char error[50];
} RenderState;

RenderState g_render_state = {0};

PreGameState g_pre_game_state = {0};
GameState g_game_state = {0};
static_assert(LENGTH(g_game_state.player_actions) ==
                  LENGTH(g_game_state.num_actions),
              "Must be the same length");

typedef struct {
  size_t indexes[MAX_CARDS_PER_PLAYER];
  size_t num_cards;
} SelectedCards;

SelectedCards get_selected_cards(void) {
  SelectedCards selected = {0};
  size_t j = 0;
  for (size_t i = 0; i < LENGTH(g_render_state.selected); ++i) {
    if (g_render_state.selected[i]) {
      assert(j < LENGTH(selected.indexes) && "Too many selected cards");
      selected.indexes[j] = i;
      ++j;
    }
  }
  selected.num_cards = j;
  return selected;
}

bool is_same_card(Card *card1, Card *card2) {
  return (memcmp(card1, card2, sizeof(*card1)) == 0);
}

bool is_valid_player_action(size_t player_idx, PlayerAction *action) {
  assert(player_idx < NUM_PLAYERS && "Wrong player index");
  for (size_t i = 0; i < g_game_state.num_actions[player_idx]; ++i) {
    if (memcmp(&g_game_state.player_actions[player_idx][i], action,
               sizeof(*action)) == 0) {
      return true;
    }
  }

  return false;
}

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

  // BACK
  sprintf(asset_path, "%s", ASSET_PATH CARD_ASSET_REL_PATH);
  strncat(asset_path, ASSET_BACK, sizeof(asset_path) - 1);
  asset = LoadTexture(asset_path);
  g_assets.back = asset;

  // BACK ROTATED
  sprintf(asset_path, "%s", ASSET_PATH CARD_ASSET_REL_PATH);
  strncat(asset_path, ASSET_BACK_ROTATED, sizeof(asset_path) - 1);
  asset = LoadTexture(asset_path);
  g_assets.back_rotated = asset;

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
  UnloadTexture(g_assets.back);
  UnloadTexture(g_assets.back_rotated);
  UnloadTexture(g_assets.background);
}

Texture2D get_background_asset(void) { return g_assets.background; }

Texture2D get_card_asset(Card card, bool show_front, bool rotated_back) {
  if (!show_front)
    return rotated_back ? g_assets.back_rotated : g_assets.back;

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

bool is_mouse_pressed(void) {
#if WASM
  return mouse_pressed > 0;
#else
  return IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
#endif
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

Rectangle get_card_rectangle(Card card, bool show_front, bool back_rotated) {
  Texture2D card_texture = get_card_asset(card, show_front, back_rotated);
  CardPose pose = get_card_pose(card);
  return (Rectangle){
      .x = pose.pos.x,
      .y = pose.pos.y,
      .width = card_texture.width * pose.scale,
      .height = card_texture.height * pose.scale,
  };
}

#define END_OF_CARDS(card) (memcmp(&card, &EMPTY_CARD, CARD_SIZE) == 0)

#define CARD_PADDING ((float)WIN_HEIHT / 30.f)
#define CARD_SPACING ((float)WIN_WIDTH / 30.f)
#define CARD_SPACING_NPC ((float)WIN_WIDTH / 40.f)
#define CARD_SCALE 1.f
#define CARD_SCALE_NPC 0.85f

#define PLAYING_PLAYER_INDEX 2

unsigned long get_num_cards(Card hand[MAX_CARDS_PER_PLAYER]) {
  for (unsigned long i = 0; i < MAX_CARDS_PER_PLAYER; ++i) {
    if (END_OF_CARDS(hand[i]))
      return i;
  }
  return MAX_CARDS_PER_PLAYER;
}

void update_hands(void) {
  memset(g_render_state.visible, 0, sizeof(g_render_state.visible));
  memset(g_render_state.selectable, 0, sizeof(g_render_state.selectable));
  memset(g_render_state.rotated_back, 0, sizeof(g_render_state.rotated_back));
  memset(g_render_state.show_front, 1, sizeof(g_render_state.show_front));

  for (unsigned long i = 0; i < LENGTH(g_game_state.game.hands); ++i) {
    unsigned long num_cards = get_num_cards(g_game_state.game.hands[i]);
    for (unsigned long j = 0; j < num_cards; ++j) {
      Card card = g_game_state.game.hands[i][j];
      size_t index = get_card_index(card);

      g_render_state.visible[index] = true;

      if ((long long)index ==
          (long long)g_render_state.selected_card_idx_mouse) {
        // Do not update pose if it is selected
        continue;
      }

      set_highest_prio(index);

      switch (i) {
      case 0: {
        // Top player
        float scale = CARD_SCALE_NPC;
        float spacing = CARD_SPACING_NPC;
        g_render_state.card_pose[index].scale = scale;
        float cards_width =
            (float)g_assets.back.width * scale + spacing * (num_cards - 1);
        g_render_state.show_front[index] = false;
        g_render_state.card_pose[index].pos.x =
            (float)WIN_WIDTH / 2.f - cards_width / 2.f + spacing * (float)j;
        g_render_state.card_pose[index].pos.y = CARD_PADDING;
      } break;
      case 1: {
        // Right player
        float scale = CARD_SCALE_NPC;
        float spacing = CARD_SPACING_NPC;
        g_render_state.card_pose[index].scale = scale;
        float cards_width = (float)g_assets.back_rotated.height * scale +
                            spacing * (num_cards - 1);
        g_render_state.show_front[index] = false;
        g_render_state.rotated_back[index] = true;
        g_render_state.card_pose[index].pos.x =
            (float)WIN_WIDTH - (float)g_assets.back_rotated.width * scale -
            CARD_PADDING;
        g_render_state.card_pose[index].pos.y =
            (float)WIN_HEIHT / 2.f - cards_width / 2.f + spacing * (float)j;
      } break;
      case 2: {
        // Bottom player
        float scale = CARD_SCALE;
        float spacing = CARD_SPACING;
        g_render_state.card_pose[index].scale = scale;
        Texture2D card_asset = get_card_asset(card, false, false);
        float cards_width =
            (float)card_asset.width * scale + spacing * (num_cards - 1);
        g_render_state.selectable[index] = true;
        g_render_state.show_front[index] = true;
        g_render_state.card_pose[index].pos.x =
            (float)WIN_WIDTH / 2.f - cards_width / 2.f + spacing * (float)j;
        g_render_state.card_pose[index].pos.y =
            (float)WIN_HEIHT - (float)card_asset.height * scale - CARD_PADDING;
      } break;
      case 3: {
        // Left player
        float scale = CARD_SCALE_NPC;
        float spacing = CARD_SPACING_NPC;
        g_render_state.card_pose[index].scale = scale;
        float cards_width = (float)g_assets.back_rotated.height * scale +
                            spacing * (num_cards - 1);
        g_render_state.show_front[index] = false;
        g_render_state.rotated_back[index] = true;
        g_render_state.card_pose[index].pos.x = CARD_PADDING;
        g_render_state.card_pose[index].pos.y =
            (float)WIN_HEIHT / 2.f - cards_width / 2.f + spacing * (float)j;
      } break;
      default:
        assert(0 && "Too many players.");
      }
    }
  }
}

/// NOTE: This function implements the logic of moving the card with the mouse
void update_card_position_mouse(void) {
  static Vector2 previous_mouse_touch = {0};
  if (is_mouse_down()) {
    Vector2 mouse_touch = get_mouse_position();
    if (g_render_state.selected_card_idx_mouse >= 0) {
      g_render_state.card_pose[g_render_state.selected_card_idx_mouse].pos.x +=
          mouse_touch.x - previous_mouse_touch.x;
      g_render_state.card_pose[g_render_state.selected_card_idx_mouse].pos.y +=
          mouse_touch.y - previous_mouse_touch.y;
      set_highest_prio(g_render_state.selected_card_idx_mouse);
    } else {
      for (size_t i = 0; i < LENGTH(g_render_state.render_prio); ++i) {
        size_t idx = g_render_state.render_prio[i];
        Card card = get_card_from_index(idx);
        Rectangle card_rec =
            get_card_rectangle(card, g_render_state.show_front[idx],
                               g_render_state.rotated_back[idx]);
        if (g_render_state.selectable[idx] &&
            CheckCollisionPointRec(mouse_touch, card_rec)) {
          g_render_state.selected_card_idx_mouse = idx;
          set_highest_prio(idx);
          break;
        }
      }
    }
    previous_mouse_touch = mouse_touch;
  } else {
    g_render_state.selected_card_idx_mouse = -1;
  }
}

void select_card(void) {
  if (is_mouse_pressed()) {
    Vector2 mouse_touch = GetMousePosition();
    for (size_t i = 0; i < LENGTH(g_render_state.render_prio); ++i) {
      size_t idx = g_render_state.render_prio[i];
      Card card = get_card_from_index(idx);
      Rectangle card_rec =
          get_card_rectangle(card, g_render_state.show_front[idx],
                             g_render_state.rotated_back[idx]);
      if (g_render_state.selectable[idx] &&
          CheckCollisionPointRec(mouse_touch, card_rec)) {
        g_render_state.selected[idx] ^= true;
        STRBUFFCPY(g_render_state.error, "");
        break;
      }
    }
  }
}

#define BOX_CHAR_SIZE CHAR_SIZE_BIG
#define BOX_FONT_SIZE FONT_SIZE_BIG
#define BOX_PADDING ((float)BOX_CHAR_SIZE / 3.5f)

#define LABEL_CHAR_SIZE_DEFAULT CHAR_SIZE_MEDIUM
#define LABEL_FONT_SIZE_DEFAULT FONT_SIZE_MEDIUM
#define LABEL_PADDING_BOX ((float)LABEL_CHAR_SIZE_DEFAULT / 2.5f)

#define TITLE_CHAR_SIZE CHAR_SIZE_BIG
#define TITLE_FONT_SIZE FONT_SIZE_BIG
#define TITLE_PADDING_BOX (3.f * BOX_CHAR_SIZE)

#define BUTTON_CHAR_SIZE CHAR_SIZE_MEDIUM
#define BUTTON_FONT_SIZE FONT_SIZE_MEDIUM
#define BUTTON_PADDING ((float)BUTTON_CHAR_SIZE / 3.5f)
#define BUTTON_PADDING_BOX (1.f * BOX_CHAR_SIZE)
#define BUTTON_WIDTH ((float)WIN_WIDTH / 4.5f)
#define BUTTON_HEIGHT ((float)BUTTON_CHAR_SIZE + 3.f * BUTTON_PADDING)
#define BUTTON_ROUNDNESS 0.25f
#define BUTTON_SEGEMENTS 0
#define BUTTON_LINE_THICKNESS 0.5f

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

  STRBUFFCPY(g_pre_game_state.title, "Welcome to Ultimate Tichu");
  g_pre_game_state.title_y = title_y;

  g_pre_game_state.button =
      (Rectangle){button_x, button_y, BUTTON_WIDTH, BUTTON_HEIGHT};
  STRBUFFCPY(g_pre_game_state.button_text, "Play");
}

void set_pre_game_state_team_names(void) {
  int number_of_text_boxes = NUM_TEAMS;
  float height = (float)BOX_CHAR_SIZE + 2.f * BOX_PADDING;
  float dy = (float)WIN_HEIHT / 12;
  float title_y = get_title_y(number_of_text_boxes, height, dy);
  float width = BOX_CHAR_SIZE * MAX_CHARS_INPUT;
  float x = (float)WIN_WIDTH / 2 - width / 2;
  float y = title_y + TITLE_PADDING_BOX;

  g_pre_game_state.number_of_text_boxes = number_of_text_boxes;
  g_pre_game_state.selected_text_box = 0;
  STRBUFFCPY(g_pre_game_state.title, "Choose Team Names");
  g_pre_game_state.title_y = title_y;
  STRBUFFCPY(g_pre_game_state.text_box_label[0],
             g_pre_game_state.game_config.sitting_order[0]);
  STRBUFFCAT(g_pre_game_state.text_box_label[0], " & ");
  STRBUFFCAT(g_pre_game_state.text_box_label[0],
             g_pre_game_state.game_config.sitting_order[2]);

  STRBUFFCPY(g_pre_game_state.text_box_label[1],
             g_pre_game_state.game_config.sitting_order[1]);
  STRBUFFCAT(g_pre_game_state.text_box_label[1], " & ");
  STRBUFFCAT(g_pre_game_state.text_box_label[1],
             g_pre_game_state.game_config.sitting_order[3]);
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
  STRBUFFCPY(g_pre_game_state.button_text, "Next");
}

void set_pre_game_state_score_limit(void) {
  int number_of_text_boxes = 1;
  float height = (float)BOX_CHAR_SIZE + 2.f * BOX_PADDING;
  float dy = (float)WIN_HEIHT / 12;
  float title_y = get_title_y(number_of_text_boxes, height, dy);
  float width = BOX_CHAR_SIZE * MAX_CHARS_INPUT;
  float x = (float)WIN_WIDTH / 2 - width / 2;
  float y = title_y + TITLE_PADDING_BOX;

  g_pre_game_state.number_of_text_boxes = number_of_text_boxes;
  g_pre_game_state.selected_text_box = 0;
  STRBUFFCPY(g_pre_game_state.title, "Choose Score Limit");
  g_pre_game_state.title_y = title_y;
  STRBUFFCPY(g_pre_game_state.text_box_label[0], "Max Score:");
  g_pre_game_state.text_box[0] = (Rectangle){x, y, width, height};
  STRBUFFCPY(g_pre_game_state.text_box_input[0], "1000");
  g_pre_game_state.input_char_counter[0] =
      strlen(g_pre_game_state.text_box_input[0]);

  int button_x = (float)WIN_WIDTH / 2 - (float)BUTTON_WIDTH / 2;
  int button_y = g_pre_game_state.text_box[0].y +
                 g_pre_game_state.text_box[0].height + BUTTON_PADDING_BOX;
  g_pre_game_state.button =
      (Rectangle){button_x, button_y, BUTTON_WIDTH, BUTTON_HEIGHT};
  STRBUFFCPY(g_pre_game_state.button_text, "Next");
}

void set_pre_game_state_player_names(void) {
  int number_of_text_boxes = NUM_PLAYERS;
  float height = (float)BOX_CHAR_SIZE + 2.f * BOX_PADDING;
  float dy = (float)WIN_HEIHT / 12;
  float title_y = get_title_y(number_of_text_boxes, height, dy);
  float width = BOX_CHAR_SIZE * MAX_CHARS_INPUT;
  float x = (float)WIN_WIDTH / 2 - width / 2;
  float y = title_y + TITLE_PADDING_BOX;

  g_pre_game_state.number_of_text_boxes = number_of_text_boxes;
  g_pre_game_state.selected_text_box = 0;
  STRBUFFCPY(g_pre_game_state.title, "Choose Sitting Order");
  g_pre_game_state.title_y = title_y;
  STRBUFFCPY(g_pre_game_state.text_box_label[0], "Player 1:");
  STRBUFFCPY(g_pre_game_state.text_box_label[1], "Player 2:");
  STRBUFFCPY(g_pre_game_state.text_box_label[2], "Player 3:");
  STRBUFFCPY(g_pre_game_state.text_box_label[3], "Player 4:");
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
  STRBUFFCPY(g_pre_game_state.button_text, "Next");
}

bool has_duplicates(char strings[][MAX_BYTES_INPUT], long long n) {
  for (long long i = 0; i < n - 1; ++i) {
    for (long long j = i + 1; j < n; ++j) {
      if (strcmp(strings[i], strings[j]) == 0)
        return true;
    }
  }
  return false;
}

void strip(char *dest, char *string, unsigned long max) {
  unsigned long n = strlen(string);
  if (n == 0 || max == 0)
    return;

  unsigned long start = 0;
  for (unsigned long i = start; i < n; ++i) {
    if (string[i] == ' ')
      ++start;
    else
      break;
  }

  long long end = (long long)n - 1ll;
  for (long long i = end; i >= 0; --i) {
    if (string[i] == ' ')
      --end;
    else
      break;
  }

  if ((long long)start > end) {
    dest[0] = '\0';
  } else {
    long long new_n = end - (long long)start + 1ll;
    new_n = MIN(new_n, (long long)max - 1ll);
    memmove(dest, string + start, new_n);
    dest[new_n] = '\0';
  }
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
      strip(g_pre_game_state.game_config.sitting_order[i],
            g_pre_game_state.text_box_input[i],
            sizeof(g_pre_game_state.game_config.sitting_order[i]));
      if (strlen(g_pre_game_state.game_config.sitting_order[i]) == 0) {
        error = "No empty player name allowed";
        goto error;
      }
    }
    if (has_duplicates(g_pre_game_state.game_config.sitting_order,
                       LENGTH(g_pre_game_state.game_config.sitting_order))) {
      error = "Choose unique player names";
      goto error;
    }
    set_pre_game_state_team_names();
  } break;
  case PGS_MAX_SCORE: {
    for (int i = 0; i < NUM_TEAMS; ++i) {
      strip(g_pre_game_state.game_config.team_names[i],
            g_pre_game_state.text_box_input[i],
            sizeof(g_pre_game_state.game_config.team_names[i]));
      if (strlen(g_pre_game_state.game_config.team_names[i]) == 0) {
        error = "No empty team names allowed";
        goto error;
      }
    }
    if (has_duplicates(g_pre_game_state.game_config.team_names,
                       LENGTH(g_pre_game_state.game_config.team_names))) {
      error = "Choose unique team names";
      goto error;
    }
    set_pre_game_state_score_limit();
  } break;
  case PGS_FINISHED: {
    if (strlen(g_pre_game_state.text_box_input[0]) == 0) {
      error = "No empty score allowed";
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

    serialize_game_config(&g_pre_game_state.game_config,
                          &g_pre_game_state.game_config_json);

  } break;
  default:
    assert(0 && "Unreachable.");
  }
  g_pre_game_state.error[0] = '\0'; // No error happened
  return;
error:
  STRBUFFCPY(g_pre_game_state.error, error);
  --g_pre_game_state.phase;
}

void reset_global_pre_game_state(void) {

  memset(&g_pre_game_state, 0, sizeof(g_pre_game_state));
  set_new_pre_game_state_phase();
  CONFIG_RESET();
}

#define PLAYER_LABEL_FONT_SIZE FONT_SIZE_MEDIUM
#define PLAYER_LABEL_CHAR_SIZE CHAR_SIZE_MEDIUM
#define PLAYER_LABEL_PADDING ((float)WIN_HEIHT / 75.f)

#define PLAYING_FIELD_WIDTH_PADDING ((float)WIN_WIDTH / 70.f)
#define PLAYING_FIELD_PADDING ((float)WIN_HEIHT / 30.f)

#define PLAY_BUTTON_WIDTH BUTTON_WIDTH
#define PLAY_BUTTON_HEIGHT BUTTON_HEIGHT
#define PLAY_BUTTON_PADDING ((float)WIN_HEIHT / 50.f)

#define TICHU_BUTTON_WIDTH BUTTON_WIDTH
#define TICHU_BUTTON_HEIGHT BUTTON_HEIGHT
#define TICHU_BUTTON_PADDING PLAY_BUTTON_PADDING

float get_card_height(void) {
  return (float)g_assets.mahjong.height * CARD_SCALE;
}

float get_card_width(void) {
  return (float)g_assets.mahjong.width * CARD_SCALE;
}

void reset_global_game_state(void) {
  memset(&g_render_state, 0, sizeof(g_render_state));
  memset(&g_game_state, 0, sizeof(g_game_state));
  g_render_state.selected_card_idx_mouse = -1; // Nothing selected

  for (size_t i = 0; i < LENGTH(g_render_state.render_prio); ++i) {
    g_render_state.render_prio[i] = i;
  }

  for (size_t i = 0; i < LENGTH(g_render_state.card_pose); ++i) {
    g_render_state.card_pose[i].scale = CARD_SCALE;
  }

  float max_cards_width = get_card_width() + CARD_SPACING * 13;
  float playing_field_width = max_cards_width + 2 * PLAYING_FIELD_WIDTH_PADDING;
  float playing_field_x = WIN_WIDTH / 2.f - playing_field_width / 2.f;
  float top_player_label_y = (float)CARD_PADDING +
                             g_assets.back.height * CARD_SCALE_NPC +
                             (float)PLAYER_LABEL_PADDING;
  float playing_field_y =
      top_player_label_y + PLAYER_LABEL_CHAR_SIZE + PLAYING_FIELD_PADDING;
  float playing_field_card_spacing = 0.5f * get_card_height();
  float playing_field_height =
      1.2f * playing_field_card_spacing + get_card_height();
  Rectangle playing_field = {.width = playing_field_width,
                             .height = playing_field_height,
                             .x = playing_field_x,
                             .y = playing_field_y};

  g_render_state.playing_field = playing_field;

  float bottom_player_label_y = (float)WIN_HEIHT - get_card_height() -
                                CARD_PADDING - PLAYER_LABEL_CHAR_SIZE -
                                PLAYER_LABEL_PADDING;
  g_render_state.top_player_label_y = top_player_label_y;
  g_render_state.bottom_player_label_y = bottom_player_label_y;

  Rectangle tichu_button = {
      playing_field_x,
      bottom_player_label_y - TICHU_BUTTON_HEIGHT - TICHU_BUTTON_PADDING,
      TICHU_BUTTON_WIDTH,
      TICHU_BUTTON_HEIGHT,
  };
  g_render_state.tichu_button = tichu_button;

  Rectangle play_button = {
      playing_field_x + playing_field_width - PLAY_BUTTON_WIDTH,
      bottom_player_label_y - PLAY_BUTTON_HEIGHT - PLAY_BUTTON_PADDING,
      PLAY_BUTTON_WIDTH,
      PLAY_BUTTON_HEIGHT,
  };
  g_render_state.play_button = play_button;


  ACTION_RESET();
}

void draw_cards(void) {
  for (int i = (int)LENGTH(g_render_state.render_prio) - 1; i >= 0; --i) {
    size_t idx = g_render_state.render_prio[i];
    if (!g_render_state.visible[idx])
      continue;

    Card card = get_card_from_index(idx);
    Texture2D card_texture = get_card_asset(
        card, g_render_state.show_front[idx], g_render_state.rotated_back[idx]);
    CardPose pose = g_render_state.card_pose[idx];
    Rectangle card_rec = get_card_rectangle(
        card, g_render_state.show_front[idx], g_render_state.rotated_back[idx]);
    if (g_render_state.selected[idx]) {
      float dy = 0.15 * card_texture.height * pose.scale;
      pose.pos.y -= dy;
      card_rec.y -= dy;
    }
    DrawTextureEx(card_texture, pose.pos, pose.rot, pose.scale, WHITE);
    float thickness = card_rec.width / 80.f;
    DrawRectangleLinesEx(card_rec, thickness, DARKGRAY);
  }
}

void reset_game(void) {
  reset_global_pre_game_state();
  reset_global_game_state();
}

void init(void) {
  InitWindow(WIN_WIDTH, WIN_HEIHT, "Tichu");
  load_global_assets();
  reset_game();
  SetTargetFPS(FPS);
}

void deinit(void) {
  for (unsigned long i = 0; i < LENGTH(g_game_state.player_actions); ++i)
    free(g_game_state.player_actions[i]);
  unload_global_assets();
  CloseWindow();
}

bool is_digit(int key) { return key >= 48 && key <= 57; }

bool is_valid_char(int key) {
  return is_digit(key) || (key >= 65 && key <= 90) ||
         (key >= 97 && key <= 122) || key == 32;
}

const char *update_draw_config(void) {
  // Update
  //----------------------------------------------------------------------------------
  if (g_pre_game_state.phase == PGS_FINISHED) {
    assert(0 && "Should not happen!");
  }

  if ((IsMouseButtonPressed(MOUSE_BUTTON_LEFT) &&
       CheckCollisionPointRec(GetMousePosition(), g_pre_game_state.button)) ||
      IsKeyPressed(KEY_ENTER)) {
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
  if (IsKeyPressed(KEY_TAB)) {
    g_pre_game_state.selected_text_box =
        (g_pre_game_state.selected_text_box + 1) %
        g_pre_game_state.number_of_text_boxes;
    g_pre_game_state.frame_counter = 0; // Trigger draw of blinking cursor
  }

  // Check if the cursor is over a textbox
  static bool was_ibeam_cursor = false;
  bool is_ibeam_cursor = false;
  for (unsigned long i = 0; i < g_pre_game_state.number_of_text_boxes; ++i) {
    if (CheckCollisionPointRec(GetMousePosition(),
                               g_pre_game_state.text_box[i])) {
      SetMouseCursor(MOUSE_CURSOR_IBEAM);
      is_ibeam_cursor = true;
      break;
    }
  }
  if (!is_ibeam_cursor && was_ibeam_cursor) {
    SetMouseCursor(MOUSE_CURSOR_DEFAULT);
  }
  was_ibeam_cursor = is_ibeam_cursor;

  // Currently selected textbox and input
  Rectangle *selected_tb =
      &g_pre_game_state.text_box[g_pre_game_state.selected_text_box];
  char(*selected_tb_input)[MAX_BYTES_INPUT] =
      &g_pre_game_state.text_box_input[g_pre_game_state.selected_text_box];
  int *selected_char_counter =
      &g_pre_game_state.input_char_counter[g_pre_game_state.selected_text_box];

  int key = GetCharPressed();
  bool (*valid_key)(int) =
      g_pre_game_state.phase == PGS_MAX_SCORE ? &is_digit : &is_valid_char;
  while (key > 0) {
    if (*selected_char_counter < MAX_CHARS_INPUT && valid_key(key)) {
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
    int font_size_label = LABEL_FONT_SIZE_DEFAULT;
    int char_size_label = LABEL_CHAR_SIZE_DEFAULT;
    if (g_pre_game_state.phase == PGS_TEAM_NAMES &&
        // We use the players names in the team name labels so if they are too
        // long we use smaller fonts
        strlen(g_pre_game_state.text_box_label[i]) > 25) {
      font_size_label = FONT_SIZE_SMALL;
      char_size_label = CHAR_SIZE_SMALL;
    }
    int text_size =
        MeasureText(g_pre_game_state.text_box_label[i], font_size_label);
    DrawText(g_pre_game_state.text_box_label[i],
             (float)WIN_WIDTH / 2.f - text_size / 2.f,
             g_pre_game_state.text_box[i].y - LABEL_PADDING_BOX -
                 char_size_label,
             font_size_label, BLACK);
  }

  // Draw blinking underscore char
  if (g_pre_game_state.number_of_text_boxes > 0 &&
      *selected_char_counter < MAX_CHARS_INPUT &&
      (g_pre_game_state.frame_counter / 20) % 2 == 0) {

    DrawText("_",
             selected_tb->x + BOX_PADDING +
                 3.f // To give some space to next char
                 + MeasureText(*selected_tb_input, BOX_FONT_SIZE),
             selected_tb->y + BOX_PADDING, BOX_FONT_SIZE, MAROON);
  }

  // Button
  DrawRectangleRounded(g_pre_game_state.button, BUTTON_ROUNDNESS,
                       BUTTON_SEGEMENTS, BLUE);
  DrawRectangleRoundedLines(g_pre_game_state.button, BUTTON_ROUNDNESS,
                            BUTTON_SEGEMENTS, BUTTON_LINE_THICKNESS, DARKBLUE);
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

void draw_labels_and_buttons(void) {
  int font_size = FONT_SIZE_SMALL;
  int char_size = CHAR_SIZE_SMALL;
  for (unsigned int i = 0;
       i < LENGTH(g_game_state.game.game_config.sitting_order); ++i) {
    char *name = g_game_state.game.game_config.sitting_order[i];

    float x, y;
    switch (i) {
    case 0: {
      x = (float)WIN_HEIHT / 2.f - MeasureText(name, font_size) / 2.f;
      y = g_render_state.top_player_label_y;
    } break;
    case 1: {

      float cards_height =
          (float)g_assets.back_rotated.height * CARD_SCALE_NPC +
          (float)CARD_SPACING_NPC * 13;
      x = (float)WIN_WIDTH - CARD_PADDING - MeasureText(name, font_size);
      y = (float)WIN_HEIHT / 2.f - PLAYER_LABEL_CHAR_SIZE - cards_height / 2.f -
          PLAYER_LABEL_PADDING;
    } break;
    case 2: {
      x = (float)WIN_WIDTH / 2.f - MeasureText(name, font_size) / 2.f;
      y = g_render_state.bottom_player_label_y;
      float x_error = (float)WIN_WIDTH / 2.f -
                      MeasureText(g_render_state.error, FONT_SIZE_SMALL) / 2.f;
      float y_error = y + char_size + 5;
      DrawText(g_render_state.error, x_error, y_error, font_size, RED);
    } break;
    case 3: {
      float cards_height =
          (float)g_assets.back_rotated.height * CARD_SCALE_NPC +
          (float)CARD_SPACING_NPC * 13;
      x = CARD_PADDING;
      y = (float)WIN_HEIHT / 2.f - PLAYER_LABEL_CHAR_SIZE - cards_height / 2.f -
          PLAYER_LABEL_PADDING;
    } break;
    default:
      assert(0 && "Too many players");
    }
    DrawText(name, x, y, font_size, BLACK);
  }

  DrawRectangleRoundedLines(g_render_state.playing_field, 0.1, 0, 4, DARKBLUE);
  DrawRectangleRounded(g_render_state.tichu_button, BUTTON_ROUNDNESS,
                       BUTTON_SEGEMENTS, RED);
  DrawRectangleRoundedLines(g_render_state.tichu_button, BUTTON_ROUNDNESS,
                            BUTTON_SEGEMENTS, BUTTON_LINE_THICKNESS, DARKBROWN);
  PlayerAction tichu_action = {.type = CallTichu};
  if (is_valid_player_action(PLAYING_PLAYER_INDEX, &tichu_action)) {
  const char *tichu_text = "Tichu";
  DrawText(tichu_text,
           g_render_state.tichu_button.x +
               g_render_state.tichu_button.width / 2.f -
               MeasureText(tichu_text, BUTTON_FONT_SIZE) / 2.f,
           g_render_state.tichu_button.y +
               g_render_state.tichu_button.height / 2.f - BUTTON_CHAR_SIZE / 2.f,
           BUTTON_FONT_SIZE, BLACK);

  DrawRectangleRounded(g_render_state.play_button, BUTTON_ROUNDNESS,
                       BUTTON_SEGEMENTS, DARKBLUE);
  DrawRectangleRoundedLines(g_render_state.play_button, BUTTON_ROUNDNESS,
                            BUTTON_SEGEMENTS, BUTTON_LINE_THICKNESS, DARKGRAY);

  SelectedCards selected = get_selected_cards();
  const char *play_text = selected.num_cards == 0 ? "Pass" : "Play";
  DrawText(play_text,
           g_render_state.play_button.x +
               g_render_state.play_button.width / 2.f -
               MeasureText(play_text, BUTTON_FONT_SIZE) / 2.f,
           g_render_state.play_button.y +
               g_render_state.play_button.height / 2.f - BUTTON_CHAR_SIZE / 2.f,
           BUTTON_FONT_SIZE, BLACK);
  }
}

bool contain_same_cards(SelectedCards *selected,
                        Card cards[MAX_CARDS_PER_PLAYER], size_t num_cards) {
  if (selected->num_cards != num_cards)
    return false;
  bool found[MAX_CARDS_PER_PLAYER] = {0};
  for (size_t i = 0; i < selected->num_cards; ++i) {
    Card selected_card = get_card_from_index(selected->indexes[i]);
    for (size_t j = 0; j < num_cards; ++j) {
      if (is_same_card(&selected_card, &cards[j])) {
        found[i] = true;
        break;
      }
    }
  }
  for (size_t i = 0; i < selected->num_cards; ++i) {
    if (!found[i])
      return false;
  }
  return true;
}

long long player_action_from_selected(SelectedCards *selected) {
  long long idx = -1;
  PlayerAction *player_actions =
      g_game_state.player_actions[PLAYING_PLAYER_INDEX];
  size_t num_actions = g_game_state.num_actions[PLAYING_PLAYER_INDEX];
  for (size_t i = 0; i < num_actions; ++i) {
    if (player_actions[i].type == Play) {
      if (contain_same_cards(selected, player_actions[i].combination.cards,
                             player_actions[i].combination.num_cards)) {
        idx = i;
      }
    }
  }
  return idx;
}

void check_buttons(void) {
  if (is_mouse_pressed()) {
    Vector2 mouse_pos = GetMousePosition();

    if (CheckCollisionPointRec(mouse_pos, g_render_state.play_button)) {
      SelectedCards selected = get_selected_cards();
      if (selected.num_cards == 0) {
        PlayerAction action = (PlayerAction){.type = Pass};
        if (!is_valid_player_action(PLAYING_PLAYER_INDEX, &action)) {
          STRBUFFCPY(g_render_state.error, "Cannot pass!");
          return;
        }
        // TODO: Pass
      } else {
        long long player_action_idx = player_action_from_selected(&selected);
        if (player_action_idx == -1) {
          STRBUFFCPY(g_render_state.error, "Not a valid combination!");
          memset(g_render_state.selected, 0, LENGTH(g_render_state.selected));
          return;
        }
        printf("Play\n");
        // TODO: play action
      }
    }
    if (CheckCollisionPointRec(mouse_pos, g_render_state.tichu_button)) {
      PlayerAction action = (PlayerAction){.type = CallTichu};
      if (!is_valid_player_action(PLAYING_PLAYER_INDEX, &action)) {
        STRBUFFCPY(g_render_state.error, "Cannot call tichu!");
        return;
      }
      printf("Tichu\n");
    }
  }
}

const char *update_draw_game(const char *game_json) {

  parse_game_and_actions(&g_game_state, game_json);

  update_hands();
  select_card();
  check_buttons();

  BeginDrawing();
  ClearBackground(WHITE);
  DrawTexture(get_background_asset(), 0, 0, WHITE);
  draw_labels_and_buttons();
  draw_cards();

  EndDrawing();

  return g_game_state.current_action_json;
}

bool window_should_close(void) { return WindowShouldClose(); }
