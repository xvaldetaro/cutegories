### Template Purescript Project with Halogen Webpack and Tailwind
to run:
`npm install`
`npm start`

### Getting Tailwind Autocomplete in VSCode
This post explains it for Haskell, but it also applies to Purescript. All you need to do is add the following to `settings.json`:
```
"tailwindCSS.includeLanguages": {
  "purescript": "html"
},
"tailwindCSS.experimental.classRegex": [
  "css\\s+\"([^\"]*)"
]
```
Change the regex above to match whatever function you use to add CSS classes. In my case it is `css`.