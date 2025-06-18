#include "gui.c"
#include "test_json.h"

int main(void) {
  init();

  parse_game_and_actions(&g_game_state, test_json1);
  parse_game_and_actions(&g_game_state, test_json2);

  char game_config[MAX_BYTES_CONFIG_JSON] = {0};
  STRBUFFCPY(game_config, "null");
  while (!window_should_close()) {
    /* if (strcmp(game_config, "null") == 0) */
    /*   STRBUFFCPY(game_config, update_draw_config()); */
    /* else */
      update_draw_game(test_json2);
  }

  deinit();
}
