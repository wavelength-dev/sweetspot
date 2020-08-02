module.exports = function (eleventyConfig) {
  eleventyConfig.setTemplateFormats([
    "html",
    "njk",
    "css", // css is not yet a recognized template extension in Eleventy
  ]);
  eleventyConfig.addPassthroughCopy({ favicon: "." });
  eleventyConfig.addPassthroughCopy("img");
};
