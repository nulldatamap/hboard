
var Render = (function (I, T) {
  var Iterator = I.Iterator;
  T = T || { fake: true };
  // Wrap it if we don't have a terminal
  var Term = { available: T.available || false
             , put: T.put || console.log
             , putWarning: T.putWarning || console.warn
             , putError: T.putError || console.error };
  var voidf = function () { return null; };
  
  var next;
  
  var temps = {
    post: "/jtm/post.html",
    reply: "/jtm/reply.html"
  };

  var render = {};
  var asString = function ( x ) {
    if( typeof( x ) == "string" )
      return x;
    return JSON.stringify( x );
  }
  // 4 threads will by default be loaded
  var startPageCount = 4;
  // API paths for the site.
  var nullable_request = function ( url, callback ) {
    Term.put( "Getting: " + url );
    $.getJSON( url )
      .done( callback )
      .error( function() { callback( null ); } );
  };

  var site = {
    posts: function ( pindex, pcount ) { 
      return "/getposts/" + asString( pindex )
           + "/"          + asString( pcount );
    },
    preview: function ( pid ) {
      return "/getpreview/" + asString( pid );
    },
    post: function ( pid ) {
      return "/getpost/" + asString( pid );
    }
  };
  
  render.init = function ( f ) {
    next = f;
    if( Term.available )
      Term.await( init );
    else
      init();
  }
  
  init = function () {
    Template.loadTemplates( temps, _init );
  }

  // Starts rendering all the dynamic elements of the page
  var _init = function () {
    render.posts = [];
    render.loaded = 0;
    render.total = 0;
    render.postContainer = $("#postContainer");
    Term.put( "Initialized renderer" );
    if( next )
      next();
  };

  ///
  /// Data fetchers:
  ///

  var get_entries = function ( preview, amount, callback ) {
    nullable_request( site.posts( render.total, amount ), function ( data ) {
      Term.put( "Post data: " + data );
      render.total += data.length;
      for( var i in data ) {
        render.get_post( preview, data[i], function ( pst ) {
          render.loaded += 1;
          var idx = pst.id | 0;
          render.posts[idx] = pst;
          callback( pst );
        } );
      }
    } );
  };
  
  render.get_posts = function ( amount, callback ) {
    get_entries( false, amount, callback );
  };
  
  render.get_previews = function ( amount, callback ) {
    get_entries( true, amount, callback );
  };

  render.get_post = function ( preview, postId, callback ) {
    Term.putWarning( preview ? "preview" : "post" );
    nullable_request( site[preview ? "preview" : "post"]( postId ), callback );
  };

  ///
  /// Renderes:
  ///
  
  render.render_frontpage = function () {
    render.get_previews( startPageCount, render.render_post );
  }
  
  render.render_view = function () {
    var thread = window.location.hash;
    if( !thread ) {
      Term.putWarning( "No post ID to render!" );
      return;
    }
   thread = thread.substr( 1 );
   render.get_post( false, thread, render.render_post );
  }
  
  render.render_posts = function () {
    for (var i in Object.keys( render.posts ).sort() ) {
      render.render_post( render.posts[i] );
    };
  }

  render.render_post = function ( post ) {
    if( post == null ) {
      Term.putError( "ERROR: Cannot render post, failed to fetch post." );
      window.location.replace( "/404.html" );
      return;
    }
    var postValues = {
      IMG: post.img,
      TEXT: post.text,
      ID: post.id,
      REPLIES: post.replies.reduce( function ( acc, x ) {
        console.log( acc );
        console.log( x );
        return acc += Template.renderTemplate( "reply", x );
      }, "" )
    };
    if(  document.location.pathname == "/"
      || document.location.pathname == "/index.html" ) {
      postValues.PREVIEW = "true";
    }
    render.postContainer.append( Template.renderTemplate( "post", postValues ) );
  }

  // Export the renderer
  return render;
})(Iterator, Term);
