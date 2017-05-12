const path = require('path')
const webpack = require('webpack')
const isProd = process.env.NODE_ENV === 'production'

const entries = [path.join(__dirname, 'support', 'entry.js')]

const plugins = [
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  }),
]

if (isProd) {
  plugins.push(
    new webpack.LoaderOptionsPlugin({
      minimize: true,
      debug: false
    })
  )
} else {
}

const config = {
  entry: entries,
  output: {
    path: path.join(__dirname, 'static', 'js', 'dist'),
    filename: 'bundle.js',
    publicPath: '/js/dist'
  },
  plugins: plugins,
  resolve: {
    alias: {
      'react': 'preact-compat',
      'react-dom': 'preact-compat'
    }
  },
  devServer: {
    contentBase: path.join(__dirname, 'static'),
    // hot: true,
    watchContentBase: true
  }
}

module.exports = config
