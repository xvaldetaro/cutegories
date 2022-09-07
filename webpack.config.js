const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require("webpack");
const CopyPlugin = require("copy-webpack-plugin");

function normalizeName(name) {
  return name.replace(/node_modules/g, "").replace(/[\-_.|]+/g, " ")
    .replace(/\b(vendors|nodemodules|js|modules|es)\b/g, "")
    .trim().replace(/ +/g, "-");
}

module.exports = {
	mode: "development",
	entry: "./src/index.js",
	output: {
		path: path.resolve(__dirname, "docs"),
		filename: "[name].bundle.js",
	},
  // Need this because for some reason the default bundle name for firebase (node_modules_firebase_app_dist_index_esm_js.bundle)
  // causes a bug in gh-pages. The file can't be loaded for some reason. This code below changes the
  // Default bundle names so that it works.
  optimization: {
    splitChunks: {
      chunks: 'all',
      name: (module, chunks, cacheGroupKey) => {
        const allChunksNames = chunks.map((chunk) => chunk.name).join('~');
        const prefix = cacheGroupKey === 'defaultVendors' ? 'vendors' : cacheGroupKey;
        return `${prefix}~${allChunksNames}`;
      },
    },
  },
	plugins: [
		new HtmlWebpackPlugin({
			template: "public/index.html",
		}),
	],
	module: {
		rules: [
			{
				test: /\.js$/i,
				include: path.resolve(__dirname, "src"),
				use: {
					loader: "babel-loader",
					options: {
						presets: ["@babel/preset-env"],
					},
				},
			},
			{
				test: /\.s[ac]ss$/i,
				include: [path.resolve(__dirname, "src")],
				use: ["style-loader", "css-loader", "sass-loader"],
			},
			{
				test: /\.glsl/,
				include: [path.resolve(__dirname, "src")],
				type: "asset/source",
			},
			{
				test: /\.css$/i,
				include: [path.resolve(__dirname, "src")],
				use: ["style-loader", "css-loader", "postcss-loader"],
			},
		],
	},
	devServer: {
    host: '0.0.0.0',
    port: 8030,
    allowedHosts: ['all'],
    disableHostCheck: true,
		static: {
			directory: path.join(__dirname, "public"),
		},
		historyApiFallback: {
			index: "public/index.html",
		},
	},
};
