
var Iterator = (function (){
  var iterator = {};

  iterator.ArrayIter = function ( vals ) {
    this.values = vals;
    this.index = 0;
    this.has_next = function () {
      return this.index != this.values.length;
    };
    this.next = function () {
      if( this.has_next() )
        return this.values[ this.index++ ];
      else
        return null;
    };
  }

  iterator.RegexIter = function ( pattern, matchee ) {
    this.pattern = pattern;
    this.accum = matchee;
    this.index = 0;
    this.has_next = function () {
      return this.accum.search( this.pattern ) != -1;
    }
    this.next = function () {
      if( this.has_next() ) {
        var r = this.pattern.exec( this.accum );
        var offset = r.index;
        this.accum = this.accum.slice( offset + r[0].length );
        r.index += this.index;
        this.index += r[0].length + offset;
        return r;
      } else 
        return null;
    }
  }

  iterator.Iterator = function ( vals, rparm ) {
    if( vals instanceof RegExp ) {
      return new iterator.RegexIter( vals, rparm );
    }else {
      return new iterator.ArrayIter( vals );
    }
  }
  return iterator;
})();
