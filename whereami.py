import libtcodpy as libtcod
import time
from random import *

SCREEN_WIDTH = 80
SCREEN_HEIGHT = 50
LIMIT_FPS = 20

def handle_keys():
  global player_x, player_y, m, turns
  #movement keys
  if libtcod.console_is_key_pressed(libtcod.KEY_UP):
    if libtcod.map_is_walkable(m, player_x, player_y - 1):
      player_y -= 1
      turns += 1
  elif libtcod.console_is_key_pressed(libtcod.KEY_DOWN):
    if libtcod.map_is_walkable(m, player_x, player_y + 1):
      player_y += 1
      turns += 1
  elif libtcod.console_is_key_pressed(libtcod.KEY_LEFT):
    if libtcod.map_is_walkable(m, player_x - 1, player_y):
      player_x -= 1
      turns += 1
  elif libtcod.console_is_key_pressed(libtcod.KEY_RIGHT):
    if libtcod.map_is_walkable(m, player_x + 1, player_y):
      player_x += 1
      turns += 1

  key = libtcod.Key()
  mouse = libtcod.Mouse()
  libtcod.sys_check_for_event(libtcod.EVENT_ANY, key, mouse)
  if key.vk == libtcod.KEY_ENTER and key.lalt:
    #Alt+Enter: toggle fullscreen
    libtcod.console_set_fullscreen(not libtcod.console_is_fullscreen())
  elif key.vk == libtcod.KEY_SPACE:
    end(player_x, player_y, mouse.cx, mouse.cy)
  elif key.vk == libtcod.KEY_ESCAPE:
    return True  #exit game

def draw_map(m, width, height):
  global player_x, player_y
  key = libtcod.Key()
  mouse = libtcod.Mouse()
  libtcod.sys_check_for_event(libtcod.EVENT_ANY, key, mouse)
  libtcod.map_compute_fov(m, player_x, player_y)
  for x in range(width):
    for y in range(height):
      if libtcod.map_is_in_fov(m, x, y):
        if libtcod.map_is_walkable(m, x, y):
          char = '.'
        else:
          char = '#'
      else:
        char = ' '
      if (mouse.cx, mouse.cy) == (x, y):
        bg = libtcod.blue
      else:
        bg = libtcod.black
      libtcod.console_put_char_ex(0, x, y, char, libtcod.white, bg)
        
def end(px, py, mx, my):
  global turns
  if (px, py)  == (mx, my):
    s = "WINNER IN " + str(turns) + " TURNS   "
  else:
    s = "LOSER IN " + str(turns) + " TURNS   "
  libtcod.console_put_char_ex(0, player_x, player_y, '@', libtcod.white, libtcod.black)

  libtcod.console_set_default_background(0, libtcod.red)
  libtcod.console_rect(0, 0, 0, SCREEN_WIDTH, 1, False, libtcod.BKGND_SET)
  libtcod.console_set_default_foreground(0, libtcod.white)
  libtcod.console_print(0, 0, 0, s)
  libtcod.console_flush()

  key = libtcod.Key()
  mouse = libtcod.Mouse()
  while True:
    libtcod.sys_check_for_event(libtcod.EVENT_ANY, key, mouse)
    if key.vk == libtcod.KEY_ESCAPE:
      return True  #exit game
  

seed(time.time())

libtcod.console_set_custom_font('arial10x10.png', libtcod.FONT_TYPE_GREYSCALE | libtcod.FONT_LAYOUT_TCOD)
libtcod.console_init_root(SCREEN_WIDTH, SCREEN_HEIGHT, 'WHERE AM I? D:', False)
libtcod.sys_set_fps(LIMIT_FPS)

width = SCREEN_WIDTH
height = SCREEN_HEIGHT

m = libtcod.map_new(width, height)
for x in range(width):
  for y in range(height):
    if x in [0, width-1] or y in [0,height-1]: # Walls
      libtcod.map_set_properties(m, x, y, False, False)
    elif x in range(randint(0,7),randint(9,20)) and y in range(6,20): # More walls
      libtcod.map_set_properties(m, x, y, False, False)
    elif x in range(randint(25,37),randint(39,55)) and y in range(6,20): # More walls
      libtcod.map_set_properties(m, x, y, False, False)
    elif x in range(randint(40,49),randint(49,70)) and y in range(6,20): # More walls
      libtcod.map_set_properties(m, x, y, False, False)
    elif x in range(randint(30,37),randint(38,70)) and y in range(6,20): # More walls
      libtcod.map_set_properties(m, x, y, False, False)
    else: # Floor
      libtcod.map_set_properties(m, x, y, True, True)

player_x = randint(1, SCREEN_WIDTH-2)
player_y = randint(SCREEN_HEIGHT/2, SCREEN_HEIGHT-2)

turns = 0
while not libtcod.console_is_window_closed():
  draw_map(m, width, height)
  exit = handle_keys()
  if exit: break
  #libtcod.console_put_char_ex(0, player_x, player_y, '@', libtcod.white, libtcod.black)
  libtcod.console_flush()
  libtcod.console_put_char_ex(0, player_x, player_y, ' ', libtcod.white, libtcod.black)

