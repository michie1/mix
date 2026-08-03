const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = {
  mode: 'development',
  entry: './static/index.js',

  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'index.js',
    clean: true
  },

  resolve: {
    extensions: ['.js', '.elm']
  },

  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {
            debug: true
          }
        }
      }
    ]
  },

  plugins: [
    new CopyPlugin({
      patterns: [
        { from: 'static/index.html', to: 'index.html' },
        { from: 'static/tracks.json', to: 'static/tracks.json' }
      ]
    })
  ],

  devServer: {
    port: 3001,
    static: {
      directory: path.join(__dirname, 'dist')
    },
    devMiddleware: {
      stats: 'errors-only'
    }
  }
};
