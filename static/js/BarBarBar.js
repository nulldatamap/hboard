function Bar() {
    var apiUrl = "http://barbarbar.me/dev/api/boards/";
    var currentUrl = window.location.pathname;  
    var $this = $(this);
    var _this = this;
    var $body= $("body");

    var path_fragments = window.location.pathname.split( "/" );
    var board = path_fragments[1];
    var thread = path_fragments[2];

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

        if ( $(".page__logo").hasClass("__bar:pagetitle__") ) {
            $(".page__logo span").html(opts.pagetitle);
        }

        $(".page__header a").attr("href", currentUrl);

        $body.css("background-color", opts.background_color);
        $body.css("color", opts.text_color);
    };
    //initialize end
    ////////////////////////////////////////////////////////////////////////////////////////

    //getJSON
    ////////////////////////////////////////////////////////////////////////////////////////
    function get(url) {
          return new Promise(function(resolve, reject) {
            var req = new XMLHttpRequest();
            req.open('GET', url);

            req.onload = function() {
              if (req.status == 200) {
                resolve(req.response);
              }
              else {
                reject(Error(req.statusText));
              }
            };

            req.onerror = function() {
              reject(Error("Network Error"));
            };

            req.send();
          });
        }

        function getJSON(url) {
          return get(url).then(JSON.parse);
        }
        //getJSON end
        ////////////////////////////////////////////////////////////////////////////////////////

    var checkView = function() {
        if ($body.hasClass("boardView")) {
            getPosts();
        }

        if ($body.hasClass("threadView")) {
            getThread();
        }
    }
    
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

        if ( $("nav").hasClass("__bar:topnav__")) {

            barget.done(function( data ) {
                for(name in data) {
                    $("#MainNavigation ul").append(
                        "<li><a href='boardview.html#" + name + "' onClick='reloadPage();'>" + name + " - " + data[name] +
                        "</a></li>"
                    );
                }
            }); 
        }
    };
    //createNav end
    ////////////////////////////////////////////////////////////////////////////////////////

    /*var get_the_post = function() {
        getJSON(apiUrl + "a/").then(function(data) {
            var the_id = [];
            for ( var i in data ) {
                var key = data[i];
                the_id.push(data[i]);
                var goUpOne = 0;
                getJSON(apiUrl + "a/" + key + "/").then(function(post) {
                    
                    console.log(the_id);
                    console.log(i + " the id = " + the_id[goUpOne]);
                    goUpOne++;
                });
            }
        });
    }*/

    ///////////////////////////////////////////////////////////////////////////////////////
    //getPosts start
    //Gets the posts for the current board

    var getPosts = function() {

        getJSON(apiUrl + board + "/").then(function(data) {
            var the_id = [];
            for (var i in data) {
                var key = data[i];

                the_id.push(key);
                //console.log(the_id);
                var j = 0;
                getJSON(apiUrl + board + "/" + key + "/").then(function(post) {

                    var postDiv = document.createElement("div");
                    postDiv.classList.add("board__post");

                    var imageDiv = document.createElement("div");
                    var img = document.createElement("img");
                    var thread_link = document.createElement("a");
                    imageDiv.classList.add("post__image");
                    thread_link.setAttribute("href", "threadview.html#" + board + "#" + the_id[j]);
                    thread_link.innerHTML = "View thread";
                    img.src = "static/thumbnail/" + post.image;
                    imageDiv.appendChild(img);
                    postDiv.appendChild(imageDiv);
                    postDiv.appendChild(thread_link);

                    var contentDiv = document.createElement("div");
                    contentDiv.classList.add("post__text");

                    var postID = document.createElement("p");
                    postID.innerHTML = "Id: "  + post.poster_id;
                    contentDiv.appendChild(postID);

                    var text = document.createElement("p");
                    text.innerHTML = post.text; 
                    contentDiv.appendChild(text);
                    postDiv.appendChild(contentDiv);

                    document.querySelector("#contentWrapper").appendChild(postDiv);
                    j++;
                    if (post.replies === null) { // this is the worst API
                        return;
                    }

                    post.replies.reverse();
                    // now add all the replies  
                    for (var reply of post.replies) {
                        var $post = $(".board__post");
                        var replyDiv = document.createElement("div");
                        replyDiv.classList.add("post__reply");

                        var repImageDiv = document.createElement("div");
                        var img = document.createElement("img");
                        repImageDiv.classList.add("reply__image");
                        img.src = "static/thumbnail/" + reply.image;
                        repImageDiv.appendChild(img);
                        replyDiv.appendChild(repImageDiv);

                        var repContentDiv = document.createElement("div");
                        repContentDiv.classList.add("reply__text");


                        var postID = document.createElement("p");
                        postID.innerHTML = "Id: "  + reply.poster_id;
                        repContentDiv.appendChild(postID);

                        var text = document.createElement("p");
                        text.innerHTML = reply.text;
                        repContentDiv.appendChild(text);
                        replyDiv.appendChild(repContentDiv);

                        //$post.append(replyDiv);
                        //document.querySelector("#wrapper").appendChild(replyDiv);
                    }

                    postDiv.appendChild(replyDiv);
                });
            }
        });
    }
    //getPosts end
    ////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////
    //getThread start
    //Gets the thread the user clicked on
    var getThread = function() {
        
        getJSON(apiUrl + board + "/").then(function(data) {
            data.reverse();
            getJSON(apiUrl + board + "/" + thread + "/").then(function(post) {
                if (post.image === null) {
                    return;
                }
                var postDiv = document.createElement("div");
                postDiv.classList.add("board__post");

                var imageDiv = document.createElement("div"); 
                var img = document.createElement("img");
                imageDiv.classList.add("post__image");
                img.src = "static/thumbnail/" + post.image;
                imageDiv.appendChild(img); 
                postDiv.appendChild(imageDiv);

                var contentDiv = document.createElement("div");
                contentDiv.classList.add("post__text");

                var postID = document.createElement("p");
                postID.innerHTML = "Id: "  + post.poster_id;
                contentDiv.appendChild(postID);

                var text = document.createElement("p");
                text.innerHTML = post.text; 
                contentDiv.appendChild(text);
                postDiv.appendChild(contentDiv);

                document.querySelector("#wrapper").appendChild(postDiv);

                post.replies.reverse();
                    // now add all the replies  
                    for (var reply of post.replies) {
                        var $post = $(".board__post");
                        var replyDiv = document.createElement("div");
                        replyDiv.classList.add("post__reply");

                        var repImageDiv = document.createElement("div");
                        var img = document.createElement("img");
                        repImageDiv.classList.add("reply__image");
                        img.src = "static/thumbnail/" + reply.image;
                        repImageDiv.appendChild(img);
                        replyDiv.appendChild(repImageDiv);

                        var repContentDiv = document.createElement("div");
                        repContentDiv.classList.add("reply__text");


                        var postID = document.createElement("p");
                        postID.innerHTML = "Id: "  + reply.poster_id;
                        repContentDiv.appendChild(postID);

                        var text = document.createElement("p");
                        text.innerHTML = reply.text;
                        repContentDiv.appendChild(text);
                        replyDiv.appendChild(repContentDiv);

                        $post.append(replyDiv);
                        //document.querySelector("#wrapper").appendChild(replyDiv);
                    }
            });
        });
    }
    
    //getThread end
    ////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////
    //removeTemplateKeys start
    //Checks the body for any of the template tags and removes them when 
    var removeTemplateKeys = function() {
        var tempKeys = {
            topnav:     "topnav__",
            pagetitle:  "pagetitle__"
        }

        $("*").each(function() {
            for ( key in tempKeys ) {
                if( $(this).hasClass(tempKeys[key]) ) {
                    console.log(tempKeys[key] + " Has been removed");
                    $(this).removeClass("__bar:" + tempKeys[key]);
                }
            } 
        });     
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

        CheckView: function() {
            checkView();
        },
        
        //GetBoards keyword to call the getBoards function.
        GetBoards: function(container, wrapper) {
            container.append(getBoards(container, wrapper));
        },

        GetPosts: function() {
            getPosts();
        },

        //GetThread: function() {
        //    getThread();
        //}, 

        //clearKeys keyword to call the removeTemplateKeys function.
        clearKeys: function() {
            removeTemplateKeys();
        }
    }
}