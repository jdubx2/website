$(document).ready(function() {
  $(window).on("scroll", function() {
    if ($(window).scrollTop() >= 20) {
      $(".navbar").addClass("compressed");
      $(".navlogo").width(42).height(42);
      $(".navbar-brand").addClass("compressed");
    } else {
      $(".navbar").removeClass("compressed");
      $(".navlogo").width(55).height(55);
      $(".navbar-brand").removeClass("compressed");
    }
  });
});
