$(document).ready(function() {
  $(window).on("scroll", function() {
    if ($(window).scrollTop() >= 20) {
      $(".navbar").addClass("compressed");
      $(".navlogo").width(54).height(54);
      $(".navbar-brand").addClass("compressed");
    } else {
      $(".navbar").removeClass("compressed");
      $(".navlogo").width(65).height(65);
      $(".navbar-brand").removeClass("compressed");
    }
  });
});
