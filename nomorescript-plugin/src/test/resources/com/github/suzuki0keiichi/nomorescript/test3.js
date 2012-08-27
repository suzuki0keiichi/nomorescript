enchant();
var CHARA_IMAGE_NAME = "http://enchantjs.com/assets/images/chara1.gif";
window.onload = function () {
  var (window,AnyRef) = this;
  var game = (new nomorescriptroot.enchantjs_fortest.Game()).__new__();
  game.preload(CHARA_IMAGE_NAME);
  game.onload = function () {
    var (game,nomorescriptroot.enchantjs_fortest.Game) = this;
    var scene = game.rootScene;
    scene.backgroundColor = "black";
    var sprite = (new nomorescriptroot.enchantjs_fortest.Sprite()).__new__(32, 32);
    sprite.moveTo(0, 100);
    sprite.image = game.assets[CHARA_IMAGE_NAME];
    scene.addChild(sprite);
    var PLAYER_MOVE_RANGE = game.width - sprite.width;
    var frameList = [0, 1, 2];
    sprite.frameIndex = 0;
    sprite.vx = 4;
    sprite.onenterframe = function () {
      var (sprite,nomorescriptroot.enchantjs_fortest.Sprite) = this;
      sprite.x = sprite.x + sprite.vx;
      if (game.frame % 2 == 0) {
        sprite.frameIndex = sprite.frameIndex + 1;
        sprite.frameIndex = sprite.frameIndex % frameList.length;
        sprite.frame = frameList[sprite.frameIndex];
      }
      if (sprite.x < 0) {
        sprite.vx = sprite.vx * -1;
        sprite.x = 0;
        sprite.scaleX = sprite.scaleX * -1;
      } else {
        if (sprite.x > PLAYER_MOVE_RANGE) {
          sprite.vx = sprite.vx * -1;
          sprite.x = PLAYER_MOVE_RANGE;
          sprite.scaleX = sprite.scaleX * -1;
        }
      }
    };
  };
  game.start();
};
