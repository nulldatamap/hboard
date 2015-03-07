/*function smoothScrollToAnchor() {
  $('a[href*=#]:not([href=#])').click(function() {
    if (location.pathname.replace(/^\//,'') == this.pathname.replace(/^\//,'') 
        || location.hostname == this.hostname) {

        var target = $(this.hash);
        target = target.length ? target : $('[name=' + this.hash.slice(1) +']');
           if (target.length) {
             $('html,body').animate({
                 scrollTop: target.offset().top
            }, {
              duration: 800,
              easing: 'easeOutQuart' 
            });
            return false;
        }
    }
});
}*/
$(document).ready(function() {
    Bar().Init();
    Bar().Nav();
    Bar().clearKeys();
}); 