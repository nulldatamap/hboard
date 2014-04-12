
var Template = (function (I){
  var Iterator = I.Iterator;
  var template = {};
  template.loaded = 0;
  template.total = 0;
  template.templates = {};

  template.loadTemplates = function ( paths, callback ) {
    var counter = { val: Object.keys(paths).length };
    for( var n in paths ) {
      // true, because we already increased total.
      template.loadTemplate( n, paths[n], callback, counter );
    }
  }
  
  template.loadTemplate = function ( bind, path, callback, b ) {
    b = b || { val: 1 };
    template.total += b.val;
    $.ajax( path )
      .done( function ( data ) {
        console.log( "Loaded template: '" + bind + "'" );
        template.templates[bind] = data;
        b.val -= 1;
        template.loaded += 1;
        if( b.val === 0 ) {
          callback();
        } else {
          console.log( template.loaded , "/", template.total );
        }
      })
      .error( function () {
        console.log( "ERROR: Failed to load template '" + bind + "'" );
      });
  }

  template.renderTemplate = function ( tname, values ) {
    if( !template.templates.hasOwnProperty( tname ) ) {
      console.log( "ERROR: Cannot render template '" + tname + "', it doesn't exist." );
      return;
    }

    var apply = function ( pattern, text, callback ) {
      var m;
      m = pattern.exec( text );
      if( !m )
        return text;
      var head = text.slice( 0, m.index );
      var tail = text.slice( m.index + m[0].length );
      return head + callback( m ) + apply( pattern, tail, callback );
    }
    var tdefpattern = /{(\w+)=([^{}]+)}/;
    var tvarpattern = /{(\w+)}/;
    var tcondpattern = /{(\w+)(!?\?)([^}]+)}/;
    var accu = template.templates[tname];
    // Replace variable templates
    accu = apply( tdefpattern, accu, function ( match ) {
      values[match[1]] = match[2];
      return "";
    } );
    accu = apply( tvarpattern, accu, function ( match ) {
      var ret = "";
      if( values.hasOwnProperty( match[1] ) )
        ret = values[match[1]];
      else
        console.log( "WARNING: The template value '" + match[1]
                     + "' was not found, \"\" used instead." );
      return ret;
    } );
    accu = apply( tcondpattern, accu, function ( match ) {
      var condmod = [ function ( x ) { return x; }, function ( x ) { return !x; } ];
      var cond = values[match[1]];
      var condr = condmod[(match[2] != '?') + 0]( cond === "" || cond );
      if( condr ) {
        return match[3];
      }else {
        return "";
      }
    } );
    return accu;
  }
  return template;
} )(Iterator);

