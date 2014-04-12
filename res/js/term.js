
var Term = (function (T){
  var term = {};
  var buffer = [];
  var awaiting = [];
  var finished = false;
  var temps = { term: "/jtm/term.html"
              , termentry: "/jtm/termentry.html" };
  
  var init = function () {
    console.log( "Term activated." );
    $("body").prepend( T.renderTemplate( "term", {} ) );
    term.termElem = $("#term");
    term.lineNum = 0;
    for( var i in awaiting )
      awaiting[i]();
    finished = true;
  };
  
  term.putStyled = function ( msg, sty ) {
    term.lineNum += 1;
    var prt = { CONTENT: msg, STYLE: sty, LINENUM: term.lineNum };
    if( !term.termElem ) {
      buffer.push( prt );
    } else {
      while( buffer.length > 0 )
        term.termElem.append( T.renderTemplate( "termentry", buffer.pop() ) );
      term.termElem.append( T.renderTemplate( "termentry", prt ) );
    }
  }
  
  term.put = function ( msg ) {
    term.putStyled( msg, "Default" ); 
  }
  
  term.putError = function ( msg ) {
    term.putStyled( msg, "Error" );
  }
  
  term.putWarning = function ( msg ) {
    term.putStyled( msg, "Warning" );
  }
  
  term.await = function ( callback ) {
    if( finished )
      callback();
    else
      awaiting.push( callback );
  }
  T.loadTemplates( temps, init );
  return term;
})(Template);
