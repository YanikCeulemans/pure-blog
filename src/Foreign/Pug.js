var pug = require("pug");

exports.compileFileImpl = function (filePath) {
  return function () {
    return pug.compileFile(filePath);
  };
};

exports.renderFileImpl = function (filePath, locals) {
  return function () {
    return pug.renderFile(filePath, locals);
  };
};
