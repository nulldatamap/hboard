
var Render = (function () {
  var render;
  var asString = JSON.stringify;
  var startPageCount = 4;
  var site = {
    // int, int -> JSON?
    "posts" : function ( pindex, pcount ) { 
      return "/getposts/" + asString( pindex )
           + "/"          + asString( pcount );
    },
    // int -> JSON?
    "post" : function ( pid ) {
      return "/getpost/" + asString( pid );
    }
  };
  // Starts rendering all the dynamic elements of the page
  render.init = function () {
    render.posts = [];
    render.getPage( 0 ); // First page
    render.renderPosts();
  };

  render.get_page = function ( pageNumber ) {
    $.getJSON( "" );
  };

  render.get_post = function ( postId ) {

  };
})();
