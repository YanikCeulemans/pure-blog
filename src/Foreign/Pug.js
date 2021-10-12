var pug = require("pug");

exports.compileFileImpl = function (filePath) {
  return function () {
    return pug.compileFile(filePath);
  };
}

exports.renderImpl = function (locals, compiledPug) {
  return compiledPug(locals);
}

exports.compileFileCurriedImpl = function (filePath) {
  return function () {
    return pug.compileFile(filePath);
  };
}
