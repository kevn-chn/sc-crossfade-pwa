const equal = require('fast-deep-equal');
const purgecss = require('@fullhuman/postcss-purgecss');

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
      purgecss({
        content: ['./**/*.html'],
      }),
    ];

    return config;
  }
}
