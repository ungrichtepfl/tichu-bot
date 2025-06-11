#include "gui.c"

int main(void) {
  init();

  parse_game_and_actions(&g_game_state, test_json1);
  parse_game_and_actions(&g_game_state, test_json2);

  while (!window_should_close()) {
    update_draw_game(test_json1);
  }

  deinit();
}
