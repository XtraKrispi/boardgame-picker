/*eslint-env node */

module.exports = {
  config: {
    paths: {
      public: 'dist',
      watched: ["assets", "src"]
    },
    server: {
      port: 3000
    },
    files: {
      javascripts: {
        joinTo: "other.js"
      },
      stylesheets: {
        joinTo: "app.css"
      }
    },
    plugins: {
      elmBrunch: {
        mainModules: ["src/App.elm"],
        outputFolder: "dist/"
      },
      sass: {
        options: {
          includePaths: ['assets']
        }
      }
    }
  }
};
