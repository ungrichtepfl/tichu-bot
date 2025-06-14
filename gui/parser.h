#ifndef PARSER_H
#define PARSER_H

#include "game.h"
#include "jsmn.h"
#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_JSON_TOKENS (1 << 12)
jsmn_parser json_parser;
jsmntok_t json_tokens[NUM_JSON_TOKENS];
#define SAFECPY(g, t, n) strncpy(g, t, MIN(sizeof(g) - 1, (unsigned long)n))

#define MAX(a, b) (a > b ? a : b)
#define MIN(a, b) (a < b ? a : b)
#define LENGTH(a) (sizeof(a) / sizeof(a[0]))

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

ptrdiff_t parse_playing_card(Card *card, jsmntok_t *game_token,
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

ptrdiff_t parse_tichu_combination_tag(TichuCombination *tichu_combination,
                                      jsmntok_t *game_token,
                                      const char *game_json) {
  jsmntok_t *current_token = game_token;

  if (json_str_equal(game_json, current_token, "SingleCard")) {
    tichu_combination->type = SingleCard;
  } else if (json_str_equal(game_json, current_token, "Pair")) {
    tichu_combination->type = Pair;
  } else if (json_str_equal(game_json, current_token, "ThreeOfAKind")) {
    tichu_combination->type = ThreeOfAKind;
  } else if (json_str_equal(game_json, current_token, "Straight")) {
    tichu_combination->type = Straight;
  } else if (json_str_equal(game_json, current_token, "FullHouse")) {
    tichu_combination->type = FullHouse;
  } else if (json_str_equal(game_json, current_token, "Stairs")) {
    tichu_combination->type = Stairs;
  } else if (json_str_equal(game_json, current_token, "Bomb")) {
    tichu_combination->type = Bomb;
  } else {
    fprintf(stderr, "Unknown tichu combination: %.*s\n",
            current_token->end - current_token->start,
            game_json + current_token->start);
    assert(0);
  }
  ++current_token;

  return current_token - game_token;
}

ptrdiff_t
parse_tichu_combination_content_cards(TichuCombination *tichu_combination,
                                      jsmntok_t *game_token,
                                      const char *game_json) {
  jsmntok_t *current_token = game_token;

  assert(current_token->type == JSMN_ARRAY &&
         "The tichu cards must be an array.");
  int array_size = current_token->size;
  assert((unsigned long)array_size <= LENGTH(tichu_combination->cards) &&
         "Too many cards.");
  ++current_token;

  for (int a = 0; a < array_size; ++a) {
    current_token += parse_playing_card(&tichu_combination->cards[a],
                                        current_token, game_json);
  }

  return current_token - game_token;
}

ptrdiff_t parse_tichu_combination_content(TichuCombination *tichu_combination,
                                          jsmntok_t *game_token,
                                          const char *game_json) {
  jsmntok_t *current_token = game_token;

  assert(current_token->type == JSMN_ARRAY &&
         "Tichu combination must be an array.");

  int array_size = current_token->size;

  ++current_token;

  if (array_size == 1) {
    // NOTE: There seems to only one array (No array in array)
    current_token += parse_playing_card(&tichu_combination->cards[0],
                                        current_token, game_json);
  } else if (array_size == 2) {

    current_token += parse_tichu_combination_content_cards(
        tichu_combination, current_token, game_json);
    if (json_str_equal(game_json, current_token, "Two")) {
      tichu_combination->value = 2;
    } else if (json_str_equal(game_json, current_token, "Three")) {
      tichu_combination->value = 3;
    } else if (json_str_equal(game_json, current_token, "Four")) {
      tichu_combination->value = 4;
    } else if (json_str_equal(game_json, current_token, "Five")) {
      tichu_combination->value = 5;
    } else if (json_str_equal(game_json, current_token, "Six")) {
      tichu_combination->value = 6;
    } else if (json_str_equal(game_json, current_token, "Seven")) {
      tichu_combination->value = 7;
    } else if (json_str_equal(game_json, current_token, "Eight")) {
      tichu_combination->value = 8;
    } else if (json_str_equal(game_json, current_token, "Nine")) {
      tichu_combination->value = 9;
    } else if (json_str_equal(game_json, current_token, "Ten")) {
      tichu_combination->value = 10;
    } else if (json_str_equal(game_json, current_token, "Jack")) {
      tichu_combination->value = 11;
    } else if (json_str_equal(game_json, current_token, "Queen")) {
      tichu_combination->value = 12;
    } else if (json_str_equal(game_json, current_token, "King")) {
      tichu_combination->value = 13;
    } else if (json_str_equal(game_json, current_token, "Ace")) {
      tichu_combination->value = 14;
    } else {
      fprintf(stderr, "Unknown card value: %.*s\n",
              current_token->end - current_token->start,
              game_json + current_token->start);
      assert(0);
    }
    ++current_token;
  } else {
    fprintf(stderr, "Unknown content size %d\n", array_size);
    assert(0);
  }

  return current_token - game_token;
}

ptrdiff_t parse_tichu_combination(TichuCombination *tichu_combination,
                                  jsmntok_t *game_token,
                                  const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_OBJECT &&
         "The tichu combination must be an object.");
  int object_size = current_token->size;
  ++current_token;

  if (object_size == 1) {
    assert(json_str_equal(game_json, current_token, "tag") &&
           "If only one key it must be tag.");
    ++current_token;
    current_token += parse_tichu_combination_tag(tichu_combination,
                                                 current_token, game_json);
  } else {
    unsigned long parsed_keys_mask = 0;
    unsigned long parsed_keys_mask_cmp = 0;
    for (int o = 0; o < object_size; ++o) {
      jsmntok_t *key = current_token;
      ++current_token;
      if (json_str_equal(game_json, key, "contents")) {
        parsed_keys_mask |= 1 << 0;
        current_token += parse_tichu_combination_content(
            tichu_combination, current_token, game_json);

      } else if (json_str_equal(game_json, key, "tag")) {
        parsed_keys_mask |= 1 << 1;
        current_token += parse_tichu_combination_tag(tichu_combination,
                                                     current_token, game_json);
      } else {
        fprintf(stderr, "Unknown key: %.*s\n", key->end - key->start,
                game_json + key->start);
        assert(0);
      }
      parsed_keys_mask_cmp |= 1 << o;
    }
    assert(parsed_keys_mask == parsed_keys_mask_cmp &&
           "Some keys have not been parsed.");
  }
  return current_token - game_token;
}

ptrdiff_t parse_board(Game *game, jsmntok_t *game_token,
                      const char *game_json) {

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
ptrdiff_t parse_finish_order(Game *game, jsmntok_t *game_token,
                             const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_ARRAY && "Finish order must be an array.");
  int array_size = current_token->size;
  assert((unsigned long)array_size <= LENGTH(game->finish_order) &&
         "Finish order too long.");
  ++current_token;
  for (int a = 0; a < array_size; ++a) {
    assert(current_token->type == JSMN_STRING &&
           "Finish order must be a string (player name).");
    SAFECPY(game->finish_order[a], game_json + current_token->start,
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

ptrdiff_t parse_game_phase_tag(Game *game, jsmntok_t *game_token,
                               const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_STRING &&
         "Game phase tag must be a string.");
  if (json_str_equal(game_json, current_token, "Starting")) {
    game->game_phase.type = Starting;
  } else if (json_str_equal(game_json, current_token, "Dealing")) {
    game->game_phase.type = Dealing;
  } else if (json_str_equal(game_json, current_token, "Distributing")) {
    game->game_phase.type = Distributing;
  } else if (json_str_equal(game_json, current_token, "Playing")) {
    game->game_phase.type = Playing;
  } else if (json_str_equal(game_json, current_token,
                            "GiveAwayLooserTricksAndHands")) {
    game->game_phase.type = GiveAwayLooserTricksAndHands;
  } else if (json_str_equal(game_json, current_token, "Scoring")) {
    game->game_phase.type = Scoring;
  } else if (json_str_equal(game_json, current_token, "NextRound")) {
    game->game_phase.type = NextRound;
  } else if (json_str_equal(game_json, current_token, "Finished")) {
    game->game_phase.type = Finished;
  } else {
    fprintf(stderr, "Unknown game phase: %.*s\n",
            current_token->end - current_token->start,
            game_json + current_token->start);
    assert(0);
  }
  ++current_token;
  return current_token - game_token;
}

ptrdiff_t parse_game_phase_content(Game *game, jsmntok_t *game_token,
                                   const char *game_json) {
  jsmntok_t *current_token = game_token;
  assert(current_token->type == JSMN_ARRAY &&
         "Game phase content must be an array.");
  int array_size = current_token->size;
  ++current_token;
  if (array_size == 2 && current_token->type == JSMN_STRING &&
      (current_token + 1)->type == JSMN_PRIMITIVE) {
    // The Playing Phase
    SAFECPY(game->game_phase.player_name, game_json + current_token->start,
            current_token->end - current_token->start);
    ++current_token;
    game->game_phase.num_passes = str_to_int(game_json + current_token->start);
    ++current_token;
  } else if (current_token->type == JSMN_OBJECT) {
    // The Dealing Phase
    assert(array_size == LENGTH(game->game_phase.cards) &&
           "To many or too little cards");
    for (int a = 0; a < array_size; ++a) {
      current_token += parse_playing_card(&game->game_phase.cards[a],
                                          current_token, game_json);
    }
  } else {
    fprintf(stderr, "Unknown type for game phase content: %d\n",
            current_token->type);
    assert(0);
  }
  return current_token - game_token;
}

ptrdiff_t parse_game_phase(Game *game, jsmntok_t *game_token,
                           const char *game_json) {
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

ptrdiff_t parse_game_config(Game *game, jsmntok_t *game_token,
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

      game->game_config.score_limit =
          str_to_int(game_json + current_token->start);
      ++current_token;
    } else if (json_str_equal(game_json, key, "sittingOrder")) {
      parsed_keys_mask |= 1 << 1;
      assert(current_token->type == JSMN_ARRAY &&
             "Sitting order must be an array.");
      int array_size = current_token->size;
      ++current_token;
      assert(LENGTH(game->game_config.sitting_order) == array_size &&
             "Too many or too little elements for the sitting order.");
      for (int a = 0; a < array_size; ++a) {
        assert(current_token->type == JSMN_STRING &&
               "Element of sitting order must be a string.");
        SAFECPY(game->game_config.sitting_order[a],
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
      assert(LENGTH(game->game_config.team_names) == array_size &&
             "Too many or too little elements for the team names.");
      for (int a = 0; a < array_size; ++a) {
        assert(current_token->type == JSMN_STRING &&
               "Element of team names must be a string.");
        SAFECPY(game->game_config.team_names[a],
                game_json + current_token->start,
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

ptrdiff_t
parse_hands(Game *game, jsmntok_t *game_token, const char *game_json,
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
    assert((unsigned long)array_size <= LENGTH(game->hands[0]) &&
           "Array size must be smaller or equal the hands array.");
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

ptrdiff_t parse_scores(Game *game, jsmntok_t *game_token, const char *game_json,
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

ptrdiff_t
parse_tichus(Game *game, jsmntok_t *game_token, const char *game_json,
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

ptrdiff_t
parse_tricks(Game *game, jsmntok_t *game_token, const char *game_json,
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

ptrdiff_t parse_player_action_tag(PlayerAction *player_action,
                                  jsmntok_t *game_token,
                                  const char *game_json) {
  jsmntok_t *current_token = game_token;

  if (json_str_equal(game_json, current_token, "Pass")) {
    player_action->type = Pass;
  } else if (json_str_equal(game_json, current_token, "Play")) {
    player_action->type = Play;
  } else if (json_str_equal(game_json, current_token, "CallTichu")) {
    player_action->type = CallTichu;
  } else if (json_str_equal(game_json, current_token, "CallGrandTichu")) {
    player_action->type = CallGrandTichu;
  } else if (json_str_equal(game_json, current_token, "Stop")) {
    player_action->type = Stop;
  } else {
    fprintf(stderr, "Unknown player action: %.*s\n",
            current_token->end - current_token->start,
            game_json + current_token->start);
    assert(0);
  }
  ++current_token;

  return current_token - game_token;
}

ptrdiff_t parse_player_action_content(PlayerAction *player_action,
                                      jsmntok_t *game_token,
                                      const char *game_json) {

  jsmntok_t *current_token = game_token;
  current_token += parse_tichu_combination(&player_action->combination,
                                           current_token, game_json);

  return current_token - game_token;
}

ptrdiff_t parse_player_action(PlayerAction *player_action,
                              jsmntok_t *game_token, const char *game_json) {
  jsmntok_t *current_token = game_token;

  assert(current_token->type =
             JSMN_OBJECT && "The player action must be an object.");
  int object_size = current_token->size;
  ++current_token;

  if (object_size == 1) {
    assert(json_str_equal(game_json, current_token, "tag") &&
           "If only one key it must be tag.");
    ++current_token;
    current_token +=
        parse_player_action_tag(player_action, current_token, game_json);
  } else {
    for (int o = 0; o < object_size; ++o) {
      jsmntok_t *key = current_token;
      ++current_token;

      unsigned long parsed_keys_mask = 0;
      unsigned long parsed_keys_mask_cmp = 0;
      if (json_str_equal(game_json, key, "contents")) {
        parsed_keys_mask |= 1 << 0;
        current_token += parse_player_action_content(player_action,
                                                     current_token, game_json);

      } else if (json_str_equal(game_json, key, "tag")) {
        parsed_keys_mask |= 1 << 1;
        current_token +=
            parse_player_action_tag(player_action, current_token, game_json);
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

ptrdiff_t parse_game_actions(GameState *game_state, jsmntok_t *game_token,
                             const char *game_json) {
  jsmntok_t *current_token = game_token;
  if (current_token->type == JSMN_PRIMITIVE) {
    assert(*(game_json + current_token->start) == 'n' &&
           "Player actions must be null");
    for (unsigned long i = 0; i < LENGTH(game_state->player_actions); ++i) {
      free(game_state->player_actions[i]);
      game_state->num_actions[i] = 0;
    }
    ++current_token;
  } else if (current_token->type == JSMN_OBJECT) {
    int object_size = current_token->size;
    assert((unsigned long)object_size == LENGTH(game_state->player_actions) &&
           "There must be actions for each players");
    ++current_token;

    unsigned long parsed_keys_mask = 0;
    unsigned long parsed_keys_mask_cmp = 0;
    for (int o = 0; o < object_size; ++o) {

      jsmntok_t *key = current_token;
      assert(key->type == JSMN_STRING && "The key must be a string.");
      int idx = get_sitting_order_index(
          game_state->game.game_config.sitting_order, game_json + key->start,
          key->end - key->start);
      parsed_keys_mask |= 1 << idx;
      parsed_keys_mask_cmp |= 1 << o;
      ++current_token;

      assert(current_token->type == JSMN_ARRAY && "The  must contain an array");
      int array_size = current_token->size;
      free(game_state->player_actions[idx]);
      game_state->player_actions[idx] =
          malloc(array_size * sizeof(*game_state->player_actions[idx]));
      assert(game_state->player_actions[idx] != NULL && "Out of memory.");
      game_state->num_actions[idx] = array_size;
      ++current_token;

      for (int a = 0; a < array_size; ++a) {
        current_token += parse_player_action(
            &game_state->player_actions[idx][a], current_token, game_json);
      }
    }

    assert(parsed_keys_mask_cmp == parsed_keys_mask &&
           "Some players actions have not been parsed.");

  } else {
    fprintf(stderr, "Unknown Json Type %d for %.*s\n", current_token->type,
            current_token->end - current_token->start,
            game_json + current_token->start);
    assert(0);
  }

  return current_token - game_token;
}

ptrdiff_t parse_game(Game *game, jsmntok_t *game_token, const char *game_json) {

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
      SAFECPY(game->current_dealer, game_json + current_token->start,
              current_token->end - current_token->start);
      ++current_token;
    } else if (json_str_equal(game_json, key, "finishOrder")) {
      parsed_keys_mask |= 1 << 2;
      current_token += parse_finish_order(game, current_token, game_json);
    } else if (json_str_equal(game_json, key, "gameConfig")) {
      parsed_keys_mask |= 1 << 3;
      current_token += parse_game_config(game, current_token, game_json);
      sitting_order = &game->game_config.sitting_order;
      team_names = &game->game_config.team_names;
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
      game->should_game_stop = json_bool(game_json, current_token);
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
      assert((unsigned long)array_size <= LENGTH(game->winner_teams) &&
             "There must be less or equal winner teams than amount of teams");
      ++current_token;
      for (int a = 0; a < array_size; ++a) {
        assert(current_token->type == JSMN_STRING &&
               "The winner team must be a string.");
        SAFECPY(game->winner_teams[a], game_json + current_token->start,
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

void parse_game_and_actions(GameState *game_state, const char *game_json) {

  jsmn_init(&json_parser);
  int num_tokens = jsmn_parse(&json_parser, game_json, strlen(game_json),
                              json_tokens, NUM_JSON_TOKENS);
  jsmntok_t *current_token = &json_tokens[0];
  if (num_tokens < 0) {
    print_json_error(num_tokens);
    assert(0);
  }
  /* print_tokens(json_tokens, num_tokens, game_json, 0); */
  assert(num_tokens > 0 && "Not enough tokens.");
  assert(current_token->type == JSMN_ARRAY &&
         "First element must be an array.");
  assert(current_token->size == 2 && "There must be 2 elements in the array.");
  ++current_token;
  current_token += parse_game(&game_state->game, current_token, game_json);
  current_token += parse_game_actions(game_state, current_token, game_json);
  assert(current_token - &json_tokens[0] == (ptrdiff_t)num_tokens &&
         "Not all tokens have been parsed.");
}

#endif // PARSER_H
