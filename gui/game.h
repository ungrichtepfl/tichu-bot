#ifndef GAME_H
#define GAME_H

#include <raylib.h>
#include <stddef.h>

#define CARDS_PER_COLOR 13
#define NUM_COLORS 4
#define NUM_PLAYERS 4
#define NUM_TEAMS 2
#define TOTAL_CARDS (CARDS_PER_COLOR * NUM_COLORS + 4)
#define MAX_CARDS_PER_PLAYER (TOTAL_CARDS / 4)
#define MAX_CHARS_INPUT 15
#define MAX_BYTES_INPUT (MAX_CHARS_INPUT + 1)

#define MAX_BYTES_CURRENT_ACTION_JSON 1024
#define MAX_CHARS_CURRENT_ACTION_JSON (MAX_BYTES_CURRENT_ACTION_JSON - 1)
#define MAX_BYTES_CONFIG_JSON 1024
#define MAX_CHARS_CONFIG_JSON (MAX_BYTES_CONFIG_JSON - 1)

typedef int Passes;

typedef char PlayerName;

typedef char TeamName;

typedef int Score;

typedef int Amount;

// NOTE: IT MUST START WITH A COLOR SUCH THAT AN ALL ZEROED CARD IS INVALID
typedef enum {
  RED_CARD,
  GREEN_CARD,
  BLUE_CARD,
  BLACK_CARD,
  DRAGON,
  PHOENIX,
  MAHJONG,
  DOG,
} CardColor;

// NOTE: Card card = {0} is used as a zero terminator it is not allowed to be a
// valid card!
typedef struct {
  CardColor color;
  int number;
} Card;

const Card EMPTY_CARD = {0};
const unsigned long CARD_SIZE = sizeof(EMPTY_CARD);

typedef enum {
  SingleCard,
  Pair,
  ThreeOfAKind,
  Straight,
  FullHouse,
  Stairs,
  Bomb
} TichuCombinationType;

typedef struct {
  TichuCombinationType type;
  Card cards[MAX_CARDS_PER_PLAYER];
  int value;
} TichuCombination;

typedef struct {
  PlayerName sitting_order[NUM_PLAYERS][MAX_BYTES_INPUT];
  TeamName team_names[NUM_TEAMS][MAX_BYTES_INPUT];
  Score score_limit;
} GameConfig;

typedef enum {
  Starting,
  Dealing,
  Distributing,
  Playing,
  GiveAwayLooserTricksAndHands,
  Scoring,
  NextRound,
  Finished
} GamePhaseType;

typedef struct {
  GamePhaseType type;
  Card cards[TOTAL_CARDS];
  PlayerName player_name[MAX_BYTES_INPUT];
  Passes num_passes;
} GamePhase;

typedef enum { Tichu, GrandTichu, NoTichu } TichuType;

typedef enum {
  Pass,
  Play,
  CallTichu,
  CallGrandTichu,
  Stop,
} PlayerActionType;

typedef struct {
  PlayerActionType type;
  TichuCombination combination;
} PlayerAction;

typedef struct {
  GameConfig game_config;
  Card hands[NUM_PLAYERS][MAX_CARDS_PER_PLAYER];
  Card tricks[NUM_PLAYERS][TOTAL_CARDS];
  TichuCombination board[TOTAL_CARDS];
  GamePhase game_phase;
  TichuType tichus[NUM_PLAYERS];
  Score scores[NUM_TEAMS];
  PlayerName current_dealer[MAX_BYTES_INPUT];
  PlayerName finish_order[NUM_PLAYERS][MAX_BYTES_INPUT];
  TeamName winner_teams[NUM_TEAMS][MAX_BYTES_INPUT];
  bool should_game_stop;
} Game;

typedef enum {
  PGS_START,
  PGS_PLAYER_NAMES,
  PGS_TEAM_NAMES,
  PGS_MAX_SCORE,
  PGS_FINISHED,
} PreGameStatePhase;

typedef struct {
  GameConfig game_config;
  PreGameStatePhase phase;
  size_t number_of_text_boxes;
  size_t selected_text_box;
  float text_box_shift;
  Rectangle text_box[4];
  char title[50];
  float title_y;
  char text_box_input[4][MAX_BYTES_INPUT];
  char text_box_label[4][50];
  int input_char_counter[4];
  long long frame_counter;
  Rectangle button;
  char button_text[15];
  char error[50];
  char game_config_json[MAX_BYTES_CONFIG_JSON];
} PreGameState;

typedef struct {
  Game game;
  /// Dynamically allocated players actions
  PlayerAction *player_actions[NUM_PLAYERS];
  unsigned long long num_actions[NUM_PLAYERS];
  char error[50];
  char current_action_json[MAX_BYTES_CURRENT_ACTION_JSON];
} GameState;

#endif // GAME_H
