const defaultTheme = require("tailwindcss/defaultTheme");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{purs,js}"],
  theme: {
    boxShadow: {
      sm: "0 1px 0 rgba(4,4,5,0.2),0 1.5px 0 rgba(6,6,7,0.05),0 2px 0 rgba(4,4,5,0.05)",
      md: "0 4px 4px rgba(0,0,0,0.16)",
      lg: "0 8px 16px rgba(0,0,0,0.24)",
    },
    extend: {
      screens: {
        'kb': { 'raw': '(min-aspect-ratio: 7/9) and (max-width: 640px)' },
        // => @media (min-height: 800px) { ... }
      },
      fontFamily: {
        sans: ["Whitney", "Open Sans", ...defaultTheme.fontFamily.sans],
        title: ["Ginto", "Open Sans", ...defaultTheme.fontFamily.sans],
      },
      colors: {
        brand: "#5965F2",
        gray: {
          50: "#ECEDEE",
          100: "#DCDDDE",
          200: "#B9BBBE",
          300: "#8E9297",
          400: "#72767D",
          500: "#5C6067",
          550: "#4f545c",
          600: "#464950",
          700: "#36393F",
          800: "#2F3136",
          900: "#202225",
          950: "#040405",
        },
      },
    },
  },
  plugins: [require("@tailwindcss/forms"), require('tailwind-scrollbar')],
  variants: {
    scrollbar: ['dark', 'rounded']
  },
}
