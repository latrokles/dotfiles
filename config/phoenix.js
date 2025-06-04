// See API docs: https://kasper.github.io/phoenix/api/app
// See Example Config: https://github.com/fabiospampinato/phoenix
Phoenix.set({
  openAtLogin: true
});

// CONSTANTS
const HYPER = ['ctrl', 'alt', 'cmd'];
const HYPER_SHIFT = ['ctrl', 'alt', 'cmd', 'shift'];
const ALT_SHIFT = ['alt', 'shift'];

// GLOBALS

// WINDOW POSITIONING
Window.prototype.toTop = function() {
  this.setTopLeft({x:0, y:0});
  this.setSize({
    width: this.screen().frame().width,
    height: this.screen().frame().height / 2,
  });
};

Window.prototype.toBottom = function() {
  this.setTopLeft({x: 0, y: this.screen().frame().height / 2});
  this.setSize({
    width: this.screen().frame().width,
    height: this.screen().frame().height / 2,
  });
};

Window.prototype.toLeft = function() {
  this.setTopLeft({x: 0, y: 0});
  this.setSize({
    width: this.screen().frame().width / 2,
    height: this.screen().frame().height
  });
};

Window.prototype.toRight = function() {
  this.setTopLeft({x: this.screen().frame().width / 2, y: 0});
  this.setSize({
    width: this.screen().frame().width / 2,
    height: this.screen().frame().height
  });
};

Window.prototype.firstQuadrant = function() {
    this.setTopLeft({x: 0, y: 0});
    this.setSize({
	width: this.screen().frame().width / 2,
	height: this.screen().frame().height / 2,
    });
};

Window.prototype.secondQuadrant = function() {
    this.setTopLeft({x: this.screen().frame().width / 2, y: 0});
    this.setSize({
	width: this.screen().frame().width / 2,
	height: this.screen().frame().height / 2,
    });
};

Window.prototype.thirdQuadrant = function() {
    this.setTopLeft({x: 0, y: this.screen().frame().height / 2});
    this.setSize({
	width: this.screen().frame().width / 2,
	height: this.screen().frame().height / 2,
    });
};

Window.prototype.fourthQuadrant = function() {
    this.setTopLeft({x: this.screen().frame().width / 2, y: this.screen().frame().height / 2});
    this.setSize({
	width: this.screen().frame().width / 2,
	height: this.screen().frame().height / 2,
    });
};

// POSITIONING
Key.on('1', ALT_SHIFT, function() { Window.focused().firstQuadrant(); });
Key.on('2', ALT_SHIFT, function() { Window.focused().secondQuadrant(); });
Key.on('3', ALT_SHIFT, function() { Window.focused().thirdQuadrant(); });
Key.on('4', ALT_SHIFT, function() { Window.focused().fourthQuadrant(); });
Key.on('up', ALT_SHIFT, function() { Window.focused().toTop(); });
Key.on('down', ALT_SHIFT, function() { Window.focused().toBottom(); });
Key.on('left', ALT_SHIFT, function() { Window.focused().toLeft(); });
Key.on('right', ALT_SHIFT, function() { Window.focused().toRight(); });
Key.on('m', ALT_SHIFT, function() { Window.focused().maximise(); });
