const equal = require('fast-deep-equal');

class TailwindExtractor {
  static extract(content) {
    return content.match(/[A-Za-z0-9-_:\/]+/g) || [];
  }
}

const purgecss = require('@fullhuman/postcss-purgecss')({
  extractors: [{
    extensions: ["elm"],
    extractor: TailwindExtractor,
  }],
  content: ["./src/**/*.elm"],
  whitelist: ["html", "body"],
});

module.exports = {
  homepage: "https://kevn-chn.github.io/sc-crossfade-pwa",
  configureWebpack: (config, env) => {
    // Manipulate the config object and return it.
    const postCssLoader = config.module.rules
      .find(rule => equal(rule.test, /\.css/) || equal(rule.test, /\.css$/))
      .use.find(loader => loader.options && loader.options.ident === 'postcss');
    const plugins = postCssLoader.options.plugins();

    postCssLoader.options.plugins = () => [
      ...plugins,
      require('tailwindcss'),
      ...env === 'production' ? [purgecss] : [],
    ];

    return config;
  }
};
