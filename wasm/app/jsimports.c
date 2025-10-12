// NOTE: This file is needed as custom function cannot be imported directly
// using foreign import javascript (only builtins can be imported)
#include <stdio.h>

char *_get_user_action(void)
    __attribute__((__import_module__("env"), __import_name__("getUserAction")));

char *get_user_action(void) { return _get_user_action(); }

void _update_c_state_and_render_game(char *to_send)
    __attribute__((__import_module__("env"),
                   __import_name__("updateCStateAndRenderGame")));

void update_c_state_and_render_game(char *to_send) {
  _update_c_state_and_render_game(to_send);
}

char *_update_draw_config(void)
    __attribute__((__import_module__("env"),
                   __import_name__("updateDrawConfig")));

char *update_draw_config(void) {
  return _update_draw_config();
}

void _init(int user_player_index)
    __attribute__((__import_module__("env"), __import_name__("init")));

void init(int user_player_index) { _init(user_player_index); }

void _deinit(void)
    __attribute__((__import_module__("env"), __import_name__("deinit")));

void deinit(void) { _deinit(); }

void _new_round(void)
    __attribute__((__import_module__("env"), __import_name__("newRound")));

void new_round(void) { _new_round(); }

bool _game_should_stop(void) __attribute__((__import_module__("env"),
                                            __import_name__("gameShouldStop")));

bool game_should_stop(void) { return _game_should_stop(); }

bool _should_game_restart(void)
    __attribute__((__import_module__("env"),
                   __import_name__("shouldGameRestart")));

bool should_game_restart(void) { return _should_game_restart(); }
