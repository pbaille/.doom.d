// ==UserScript==
// @name         org-protocols
// @namespace    http://tampermonkey.net/
// @version      0.2.0
// @description  Send links or/and selected content into your Emacs via various protocols
// @match        *://*/*
// @grant        none
// ==/UserScript==

(function () {
  "use strict";
  var keys = {};

  // By pressing Ctrl + Alt + 'key':
  // capture link in org-roam via `org-protocol://roam-ref`
  keys.roam = "R";

  // stores link via `org-protocol://store-link`, which can be inserted by `org-insert-link`
  keys.link = "K";

  // stores link + title via `org-protocol://store-link`, which can be inserted by `org-insert-link`
  keys.linkTitle = "G";

  // captures selected text as an org markup via `org-protocol:///capture-html`, needs github.com/alphapapa/org-protocol-capture-html
  keys.html = "C";

  // captures link + title via `org-protocol://capture`
  keys.bookmarkTitle = "B";

  // captures link + title + selected text as a plain text via `org-protocol://capture`
  keys.bookmarkTitleSelected = "X";

  // captures selected text inside code block in Inbox
  // emacs will ask for code block language identifier and title of the item
  keys.codeInbox = "I";

  // captures selected text inside code block in Yankpad.
  // emacs will ask for code block language identifier, title of the item and category (heading) in Yankpad
  keys.codeYankpad = "Y";

  // capture html
  keys.page = "H";

  // ----------------------------------------------------------------------------------------------------

  // I need some form of sleep
  function wait(ms) {
    var start = new Date().getTime();
    var end = start;
    while (end < start + ms) {
      end = new Date().getTime();
    }
  }

  // deal with parens
  function replace_all(str, find, replace) {
    return str.replace(new RegExp(find, "g"), replace);
  }

  function escapeIt(text) {
    return replace_all(
      replace_all(replace_all(encodeURIComponent(text), "[(]", ""), "[)]", ""),
      "[']",
      ""
    );
  }

  // for that nasty case when location.href ends with slash
  const stripTrailingSlash = (str) => {
    return str.endsWith("/") ? str.slice(0, -1) : str;
  };

  // When you have parens in URL or Title... (Wikipedia!)
  function getLocation() {
    return escapeIt(stripTrailingSlash(location.href));
  }

  function getTitle() {
    return escapeIt(document.title);
  }

  // ----------------------------------------------------------------------------------------------------

  window.addEventListener("keyup", function () {
    if (!event.ctrlKey && event.altKey && !event.shiftKey) {
      switch (String.fromCharCode(event.keyCode)) {
        case keys.roam:
          // roam-ref r ref title body
          window.location =
            "org-protocol://roam-ref?template=r&ref=" +
            getLocation() +
            "&title=" +
            getTitle() +
            "&body=" +
            escapeIt(window.getSelection().toString());
          break;

        case keys.link:
          // store-link url title
          window.location = "org-protocol://store-link?url=" + getLocation();
          break;

        case keys.linkTitle:
          // store-link url title
          window.location =
            "org-protocol://store-link?url=" +
            getLocation() +
            "&title=" +
            getTitle();
          break;

        case keys.bookmarkTitle:
          // capture L url title
          window.location =
            "org-protocol://capture?template=ls&url=" +
            getLocation() +
            "&title=" +
            getTitle();
          break;

        case keys.bookmarkTitleSelected:
          // capture p url title body
          window.location =
            "org-protocol://capture?template=lt&url=" +
            getLocation() +
            "&title=" +
            getTitle() +
            "&body=" +
            escapeIt(window.getSelection().toString());
          break;

        case keys.html:
          // capture template url title body
          window.location =
            "org-protocol://capture?template=w?url=" +
            getLocation() +
            "&title=" +
            getTitle() +
            "&body=" +
            encodeURIComponent(
              (function () {
                var html = "";
                if (typeof window.getSelection != "undefined") {
                  var sel = window.getSelection();
                  if (sel.rangeCount) {
                    var container = document.createElement("div");
                    for (var i = 0, len = sel.rangeCount; i < len; ++i) {
                      container.appendChild(sel.getRangeAt(i).cloneContents());
                    }
                    html = container.innerHTML;
                  }
                } else if (typeof document.selection != "undefined") {
                  if (document.selection.type == "Text") {
                    html = document.selection.createRange().htmlText;
                  }
                }
                var relToAbs = function (href) {
                  var a = document.createElement("a");
                  a.href = href;
                  var abs =
                    a.protocol + "" + a.host + a.pathname + a.search + a.hash;
                  a.remove();
                  return abs;
                };
                var elementTypes = [
                  ["a", "href"],
                  ["img", "src"],
                ];
                var div = document.createElement("div");
                div.innerHTML = html;
                elementTypes.map(function (elementType) {
                  var elements = div.getElementsByTagName(elementType[0]);
                  for (var i = 0; i < elements.length; i++) {
                    elements[i].setAttribute(
                      elementType[1],
                      relToAbs(elements[i].getAttribute(elementType[1]))
                    );
                  }
                });
                return div.innerHTML;
              })()
            );
          break;

        case keys.codeInbox:
          // capture s url title body
          window.location =
            "org-protocol://capture?template=s&url=" +
            getLocation() +
            "&title=" +
            getTitle() +
            "&body=" +
            escapeIt(window.getSelection().toString());
          break;

        case keys.codeYankpad:
          // capture y url title body
          window.location =
            "org-protocol://capture?template=y&url=" +
            getLocation() +
            "&title=" +
            getTitle() +
            "&body=" +
            escapeIt(window.getSelection().toString());
          break;

        case keys.page:
          console.log("page");
          window.location =
            "org-protocol:///capture-html?template=w&url=" +
            encodeURIComponent(location.href) +
            "&title=" +
            encodeURIComponent(document.title || "[untitled page]") +
            "&body=" +
            encodeURIComponent(
              (function () {
                var html = "";
                if (typeof window.getSelection != "undefined") {
                  var sel = window.getSelection();
                  if (sel.rangeCount) {
                    var container = document.createElement("div");
                    for (var i = 0, len = sel.rangeCount; i < len; ++i) {
                      container.appendChild(sel.getRangeAt(i).cloneContents());
                    }
                    html = container.innerHTML;
                  }
                } else if (typeof document.selection != "undefined") {
                  if (document.selection.type == "Text") {
                    html = document.selection.createRange().htmlText;
                  }
                }
                var relToAbs = function (href) {
                  var a = document.createElement("a");
                  a.href = href;
                  var abs =
                    a.protocol + "//" + a.host + a.pathname + a.search + a.hash;
                  a.remove();
                  return abs;
                };
                var elementTypes = [
                  ["a", "href"],
                  ["img", "src"],
                ];
                var div = document.createElement("div");
                div.innerHTML = html;
                elementTypes.map(function (elementType) {
                  var elements = div.getElementsByTagName(elementType[0]);
                  for (var i = 0; i < elements.length; i++) {
                    elements[i].setAttribute(
                      elementType[1],
                      relToAbs(elements[i].getAttribute(elementType[1]))
                    );
                  }
                });
                return div.innerHTML;
              })()
            );
          break;
      }
    }
  });
})();
