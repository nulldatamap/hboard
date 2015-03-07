function Bar() {
    var apiUrl = "http://barbarbar.me/dev/api/boards/";
    var currentUrl = window.location.pathname;  
    var $this = $(this);
    var _this = this;
    var $body= $("body");

    //Define the defaults
    var defaults = {
        pagetitle:          "BarBarBar",
        background_color:   "#272727",
        text_color:         "#fff"
    };

    

    ///////////////////////////////////////////////////////////////////////////////////////
    //initialize start
    //Initialize the default stuff
    //Note: Needs to be able to take args 
    function initialize(options) {
        var opts = $.extend( {}, defaults, options );

        //Creates title tag if it's not existing.
        if ( ! $("head title").length ) {
            $("head").append("<title />");
            $("head title").html(opts.pagetitle);
        } else {
            $("head title").html(opts.pagetitle); 
        }

        if ( $(".page__logo").text().indexOf('__bar:pagetitle__') != -1) {
            $(".page__logo span").html(opts.pagetitle);
        }

        $body.css("background-color", opts.background_color);
        $body.css("color", opts.text_color);
    };
    //initialize end
    ////////////////////////////////////////////////////////////////////////////////////////
    
    ///////////////////////////////////////////////////////////////////////////////////////
    //getBoards start
    //Get all boards and post it in the tag chosen
    var getBoards = function(container, wrapper) {
        var barget = $.getJSON( apiUrl );

        if ( wrapper == undefined ) { //If the wrapper is empty print the boards anyways in a standard form
            $("body").append("<ol class='faggot__boards'>If thy is not doing ship maintenance, please use all thy arguments</ol>")

            barget.done(function(data) {
                for(name in data) {
                   $(".faggot__boards").append("<li>" + name + ' - ' + data[name] + "</li>");
               } 
           });
        } else { //Else print the boards in the desired wrapper
            barget.done(function(data) {
               for(name in data) {
                   container.append('<' + wrapper + '>' + name + ' - ' + data[name] + '</' + wrapper + '>');
               } 
            });
        }

        
    };
    //getBoards end
    ////////////////////////////////////////////////////////////////////////////////////////
    
    ///////////////////////////////////////////////////////////////////////////////////////
    //createNav start
    //Prints the boards in a list + link form.
    //Checks for keyword __bar:topnav__ to know where to put it.
    var createNav = function(container) {
            var barget = $.getJSON( apiUrl );

            if ( $("nav").text().indexOf('__bar:topnav__') != -1 ) {

            barget.done(function( data ) {
                for(name in data) {
                    $("nav ul").append(
                        "<li><a href='http://barbarbar.me/dev/api/boards/" + name + "'>" + name + " - " + data[name] +
                        "</a></li>"
                    );
                }
            });

        }
    };
    //createNav end
    ////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////
    //removeTemplateKeys start
    //Checks the body for any of the template tags and removes them when 
    var removeTemplateKeys = function() {
        var tempKeys = {
            topnav:     "topnav__",
            pagetitle:  "pagetitle__"
        }

        if ( $("body").text().indexOf('__bar:') != -1 ) {
            console.log("Spotted thy traitor!");

            for ( key in tempKeys ) {
                $("body").html($("body").html().replace("__bar:" + tempKeys[key], ""));
            }

        }
    };
    //removeTemplateKeys end
    ////////////////////////////////////////////////////////////////////////////////////////
    
    return {
        //Init keyword to call the initialize function.
        Init: function(options) {
            initialize(options);
        },

        //Nav keyword to call the createNav function.
        Nav: function(container) {
            createNav(container);
        },
        
        //GetBoards keyword to call the getBoards function.
        GetBoards: function(container, wrapper) {
            container.append(getBoards(container, wrapper));
        },

        //clearKeys keyword to call the removeTemplateKeys function.
        clearKeys: function() {
            removeTemplateKeys();
        }
    }
}