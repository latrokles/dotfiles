Phoenix.set({
  openAtLogin: true
});

// CONSTANTS 
const ALT_SHIFT = ['alt', 'shift'];

// GLOBALS

// WINDOW METHODS
const cacheWindowFrame = function(win) {
  FOCUSED_PREVIOUS_FRAME = {
    x: win.frame().x,
    y: win.frame().y,
    width: win.frame().width,
    height: win.frame().height,
  };
};

Window.prototype.toLeft = function() {
  this.setTopLeft({x: 0, y: 0});
  this.setSize({
    width: this.screen().frame().width / 2,
    height: this.screen().frame().height
  });
  cacheWindowFrame(this);
};

Window.prototype.toRight = function() {
  this.setTopLeft({x: this.screen().frame().width / 2, y: 0});
  this.setSize({
    width: this.screen().frame().width / 2,
    height: this.screen().frame().height
  });
  cacheWindowFrame(this);
};

// POSITIONING
Key.on('left', ALT_SHIFT, function() {
  Window.focused().toLeft();
});

Key.on('right', ALT_SHIFT, function() {
  Window.focused().toRight();
});

Key.on('m', ALT_SHIFT, function() {
  Window.focused().maximise();
});
