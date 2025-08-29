#include "gui.c"
#include "test_json.h"

int main(void) {
  init(USER_PLAYER_INDEX);

  parse_game_and_actions(&g_game_state, test_json);

  char game_config[MAX_BYTES_CONFIG_JSON] = {0};
  const char *game_json = NULL;
  STRBUFFCPY(game_config, "null");
  while (!window_should_close()) {
    if (strcmp(game_config, "null") == 0)
      STRBUFFCPY(game_config, update_draw_config());
    else {
      update_c_state_and_render_game(test_json);
      game_json = get_user_action();
      if (strcmp(game_json, "null") != 0) {
        break;
      }
    }
  }

  deinit();
  printf("%s\n", game_json);
}
