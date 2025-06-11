#ifndef GAME_H
#define GAME_H

#include <stdbool.h>

#define CARDS_PER_COLOR 13
#define NUM_COLORS 4
#define NUM_PLAYERS 4
#define NUM_TEAMS 2
#define TOTAL_CARDS (CARDS_PER_COLOR * NUM_COLORS + 4)
#define MAX_CARDS_PER_PLAYER (TOTAL_CARDS / 4)
#define MAX_CHARS_NAME 30
#define MAX_BYTES_NAME (MAX_CHARS_NAME + 1)

#define MAX_BYTES_CURRENT_ACTION 1024
#define MAX_CHARS_CURRENT_ACTION (MAX_BYTES_CURRENT_ACTION - 1)
#define MAX_BYTES_CURRENT_CONFIG 1024
#define MAX_CHARS_CURRENT_CONFIG (MAX_BYTES_CURRENT_CONFIG - 1)

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
  Card cards[14];
  int value;
} TichuCombination;

typedef struct {
  PlayerName sittingOrder[NUM_PLAYERS][MAX_BYTES_NAME];
  TeamName teamNames[NUM_TEAMS][MAX_BYTES_NAME];
  Score scoreLimit;
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
  PlayerName playerName[MAX_BYTES_NAME];
  Passes numPasses;
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
  GameConfig gameConfig;
  Card hands[NUM_PLAYERS][MAX_CARDS_PER_PLAYER];
  Card tricks[NUM_PLAYERS][TOTAL_CARDS];
  TichuCombination board[TOTAL_CARDS];
  GamePhase gamePhase;
  TichuType tichus[NUM_PLAYERS];
  Score scores[NUM_TEAMS];
  PlayerName currentDealer[MAX_BYTES_NAME];
  PlayerName finishOrder[NUM_PLAYERS][MAX_BYTES_NAME];
  TeamName winnerTeams[NUM_TEAMS][MAX_BYTES_NAME];
  bool shouldGameStop;
} Game;

typedef struct {
  GameConfig gameConfig;
  char game_config_json[MAX_CHARS_CURRENT_ACTION];
} PreGameState;

typedef struct {
  Game game;
  /// Dynamically allocated players actions
  PlayerAction *player_actions[NUM_PLAYERS];
  unsigned long long num_actions[NUM_PLAYERS];
  char current_action_json[MAX_CHARS_CURRENT_ACTION];
} GameState;

#endif // GAME_H
