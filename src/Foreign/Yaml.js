var yaml = require('yaml');

exports.parseImpl = function(left, right, input) {
  try {
    return right(yaml.parse(input));
  } catch (e) {
    return left(e.message);
  }
};
