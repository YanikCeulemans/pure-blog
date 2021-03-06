var hljs = require("highlight.js")
var MarkdownIt = require("markdown-it");

exports.renderString_ = function(input) {
  var md = new MarkdownIt({
    highlight: function (str, lang) {
      if (lang && hljs.getLanguage(lang)) {
        try {
          return hljs.highlight(str, { language: lang, ignoreIllegals: true }).value;
        } catch (__) {}
      }

      return "";
    }
  });
  return md.render(input);
};

exports.newMarkdownIt_ = function(preset, opts) {
  return new MarkdownIt(preset, opts);
};

exports.render_ = function(md, input) {
  return md.render(input);
};

exports.renderInline_ = function(md, input) {
  return md.renderInline(input);
};

exports.use_ = function(plugin, md) {
  return md.use(plugin);
};
