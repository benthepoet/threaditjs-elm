module.exports = {
    entry: './index.js',
    output: {
        filename: 'bundle.js'
    },
    module: {
        rules: [{
            test: /\.elm$/,
            exclude: [/elm-stuff/, /node_modules/],
            use: {
                loader: 'elm-webpack-loader',
                options: {}
            }
        }]
    },
    devServer: {
        disableHostCheck: true
    }
};