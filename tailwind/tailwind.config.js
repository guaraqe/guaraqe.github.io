/**
 * The stylesheet used to be pulled from two CDNs at page load: daisyUI's
 * full.css and cdn.tailwindcss.com, the latter being the in-browser JIT
 * compiler that Tailwind explicitly does not recommend for production. This
 * config drives a local build instead, so pages ship one small stylesheet and
 * depend on no third-party host.
 *
 * Paths are relative to the repository root, which is where the builder runs
 * the CLI from.
 */
module.exports = {
  content: [
    "./site/**/*.html",
    "./site/**/*.md",
    // Covered by the glob above, but spelled out because it is easy to miss:
    // the course pages and the note under site/raw are stored as built HTML
    // (their Markdown sources are gone) and their classes must be scanned too.
    "./site/raw/**/*.html",
  ],
  theme: {
    extend: {},
  },
  // No typography plugin: the CDN build did not load one either, so every
  // `.prose` rule on this site comes from site/css/style.css. Enabling it here
  // would silently restyle every page.
  plugins: [],
};
