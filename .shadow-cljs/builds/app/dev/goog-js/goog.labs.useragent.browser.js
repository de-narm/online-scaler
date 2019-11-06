["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/labs/useragent/browser.js"],"~:js","goog.provide(\"goog.labs.userAgent.browser\");\ngoog.require(\"goog.array\");\ngoog.require(\"goog.labs.userAgent.util\");\ngoog.require(\"goog.object\");\ngoog.require(\"goog.string.internal\");\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchOpera_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Opera\");\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchIE_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Trident\") || goog.labs.userAgent.util.matchUserAgent(\"MSIE\");\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchEdge_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Edge\");\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchFirefox_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Firefox\") || goog.labs.userAgent.util.matchUserAgent(\"FxiOS\");\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchSafari_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Safari\") && !(goog.labs.userAgent.browser.matchChrome_() || goog.labs.userAgent.browser.matchCoast_() || goog.labs.userAgent.browser.matchOpera_() || goog.labs.userAgent.browser.matchEdge_() || goog.labs.userAgent.browser.matchFirefox_() || goog.labs.userAgent.browser.isSilk() || goog.labs.userAgent.util.matchUserAgent(\"Android\"));\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchCoast_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Coast\");\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchIosWebview_ = function() {\n  return (goog.labs.userAgent.util.matchUserAgent(\"iPad\") || goog.labs.userAgent.util.matchUserAgent(\"iPhone\")) && !goog.labs.userAgent.browser.matchSafari_() && !goog.labs.userAgent.browser.matchChrome_() && !goog.labs.userAgent.browser.matchCoast_() && !goog.labs.userAgent.browser.matchFirefox_() && goog.labs.userAgent.util.matchUserAgent(\"AppleWebKit\");\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchChrome_ = function() {\n  return (goog.labs.userAgent.util.matchUserAgent(\"Chrome\") || goog.labs.userAgent.util.matchUserAgent(\"CriOS\")) && !goog.labs.userAgent.browser.matchEdge_();\n};\n/**\n * @private\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.matchAndroidBrowser_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Android\") && !(goog.labs.userAgent.browser.isChrome() || goog.labs.userAgent.browser.isFirefox() || goog.labs.userAgent.browser.isOpera() || goog.labs.userAgent.browser.isSilk());\n};\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isOpera = goog.labs.userAgent.browser.matchOpera_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isIE = goog.labs.userAgent.browser.matchIE_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isEdge = goog.labs.userAgent.browser.matchEdge_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isFirefox = goog.labs.userAgent.browser.matchFirefox_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isSafari = goog.labs.userAgent.browser.matchSafari_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isCoast = goog.labs.userAgent.browser.matchCoast_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isIosWebview = goog.labs.userAgent.browser.matchIosWebview_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isChrome = goog.labs.userAgent.browser.matchChrome_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isAndroidBrowser = goog.labs.userAgent.browser.matchAndroidBrowser_;\n/**\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isSilk = function() {\n  return goog.labs.userAgent.util.matchUserAgent(\"Silk\");\n};\n/**\n * @return {string}\n */\ngoog.labs.userAgent.browser.getVersion = function() {\n  var userAgentString = goog.labs.userAgent.util.getUserAgent();\n  if (goog.labs.userAgent.browser.isIE()) {\n    return goog.labs.userAgent.browser.getIEVersion_(userAgentString);\n  }\n  var versionTuples = goog.labs.userAgent.util.extractVersionTuples(userAgentString);\n  var versionMap = {};\n  goog.array.forEach(versionTuples, function(tuple) {\n    var key = tuple[0];\n    var value = tuple[1];\n    versionMap[key] = value;\n  });\n  var versionMapHasKey = goog.partial(goog.object.containsKey, versionMap);\n  function lookUpValueWithKeys(keys) {\n    var key = goog.array.find(keys, versionMapHasKey);\n    return versionMap[key] || \"\";\n  }\n  if (goog.labs.userAgent.browser.isOpera()) {\n    return lookUpValueWithKeys([\"Version\", \"Opera\"]);\n  }\n  if (goog.labs.userAgent.browser.isEdge()) {\n    return lookUpValueWithKeys([\"Edge\"]);\n  }\n  if (goog.labs.userAgent.browser.isChrome()) {\n    return lookUpValueWithKeys([\"Chrome\", \"CriOS\"]);\n  }\n  var tuple = versionTuples[2];\n  return tuple && tuple[1] || \"\";\n};\n/**\n * @param {(string|number)} version\n * @return {boolean}\n */\ngoog.labs.userAgent.browser.isVersionOrHigher = function(version) {\n  return goog.string.internal.compareVersions(goog.labs.userAgent.browser.getVersion(), version) >= 0;\n};\n/**\n * @private\n * @param {string} userAgent\n * @return {string}\n */\ngoog.labs.userAgent.browser.getIEVersion_ = function(userAgent) {\n  var rv = /rv: *([\\d\\.]*)/.exec(userAgent);\n  if (rv && rv[1]) {\n    return rv[1];\n  }\n  var version = \"\";\n  var msie = /MSIE +([\\d\\.]+)/.exec(userAgent);\n  if (msie && msie[1]) {\n    var tridentVersion = /Trident\\/(\\d.\\d)/.exec(userAgent);\n    if (msie[1] == \"7.0\") {\n      if (tridentVersion && tridentVersion[1]) {\n        switch(tridentVersion[1]) {\n          case \"4.0\":\n            version = \"8.0\";\n            break;\n          case \"5.0\":\n            version = \"9.0\";\n            break;\n          case \"6.0\":\n            version = \"10.0\";\n            break;\n          case \"7.0\":\n            version = \"11.0\";\n            break;\n        }\n      } else {\n        version = \"7.0\";\n      }\n    } else {\n      version = msie[1];\n    }\n  }\n  return version;\n};\n","~:source","// Copyright 2013 The Closure Library Authors. All Rights Reserved.\n//\n// Licensed under the Apache License, Version 2.0 (the \"License\");\n// you may not use this file except in compliance with the License.\n// You may obtain a copy of the License at\n//\n//      http://www.apache.org/licenses/LICENSE-2.0\n//\n// Unless required by applicable law or agreed to in writing, software\n// distributed under the License is distributed on an \"AS-IS\" BASIS,\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n// See the License for the specific language governing permissions and\n// limitations under the License.\n\n/**\n * @fileoverview Closure user agent detection (Browser).\n * @see <a href=\"http://www.useragentstring.com/\">User agent strings</a>\n * For more information on rendering engine, platform, or device see the other\n * sub-namespaces in goog.labs.userAgent, goog.labs.userAgent.platform,\n * goog.labs.userAgent.device respectively.)\n *\n * @author martone@google.com (Andy Martone)\n */\n\ngoog.provide('goog.labs.userAgent.browser');\n\ngoog.require('goog.array');\ngoog.require('goog.labs.userAgent.util');\ngoog.require('goog.object');\ngoog.require('goog.string.internal');\n\n\n// TODO(nnaze): Refactor to remove excessive exclusion logic in matching\n// functions.\n\n\n/**\n * @return {boolean} Whether the user's browser is Opera.  Note: Chromium\n *     based Opera (Opera 15+) is detected as Chrome to avoid unnecessary\n *     special casing.\n * @private\n */\ngoog.labs.userAgent.browser.matchOpera_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent('Opera');\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is IE.\n * @private\n */\ngoog.labs.userAgent.browser.matchIE_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent('Trident') ||\n      goog.labs.userAgent.util.matchUserAgent('MSIE');\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is Edge.\n * @private\n */\ngoog.labs.userAgent.browser.matchEdge_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent('Edge');\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is Firefox.\n * @private\n */\ngoog.labs.userAgent.browser.matchFirefox_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent('Firefox') ||\n      goog.labs.userAgent.util.matchUserAgent('FxiOS');\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is Safari.\n * @private\n */\ngoog.labs.userAgent.browser.matchSafari_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent('Safari') &&\n      !(goog.labs.userAgent.browser.matchChrome_() ||\n        goog.labs.userAgent.browser.matchCoast_() ||\n        goog.labs.userAgent.browser.matchOpera_() ||\n        goog.labs.userAgent.browser.matchEdge_() ||\n        goog.labs.userAgent.browser.matchFirefox_() ||\n        goog.labs.userAgent.browser.isSilk() ||\n        goog.labs.userAgent.util.matchUserAgent('Android'));\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is Coast (Opera's Webkit-based\n *     iOS browser).\n * @private\n */\ngoog.labs.userAgent.browser.matchCoast_ = function() {\n  return goog.labs.userAgent.util.matchUserAgent('Coast');\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is iOS Webview.\n * @private\n */\ngoog.labs.userAgent.browser.matchIosWebview_ = function() {\n  // iOS Webview does not show up as Chrome or Safari. Also check for Opera's\n  // WebKit-based iOS browser, Coast.\n  return (goog.labs.userAgent.util.matchUserAgent('iPad') ||\n          goog.labs.userAgent.util.matchUserAgent('iPhone')) &&\n      !goog.labs.userAgent.browser.matchSafari_() &&\n      !goog.labs.userAgent.browser.matchChrome_() &&\n      !goog.labs.userAgent.browser.matchCoast_() &&\n      !goog.labs.userAgent.browser.matchFirefox_() &&\n      goog.labs.userAgent.util.matchUserAgent('AppleWebKit');\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is Chrome.\n * @private\n */\ngoog.labs.userAgent.browser.matchChrome_ = function() {\n  return (goog.labs.userAgent.util.matchUserAgent('Chrome') ||\n          goog.labs.userAgent.util.matchUserAgent('CriOS')) &&\n      !goog.labs.userAgent.browser.matchEdge_();\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is the Android browser.\n * @private\n */\ngoog.labs.userAgent.browser.matchAndroidBrowser_ = function() {\n  // Android can appear in the user agent string for Chrome on Android.\n  // This is not the Android standalone browser if it does.\n  return goog.labs.userAgent.util.matchUserAgent('Android') &&\n      !(goog.labs.userAgent.browser.isChrome() ||\n        goog.labs.userAgent.browser.isFirefox() ||\n        goog.labs.userAgent.browser.isOpera() ||\n        goog.labs.userAgent.browser.isSilk());\n};\n\n\n/**\n * @return {boolean} Whether the user's browser is Opera.\n */\ngoog.labs.userAgent.browser.isOpera = goog.labs.userAgent.browser.matchOpera_;\n\n\n/**\n * @return {boolean} Whether the user's browser is IE.\n */\ngoog.labs.userAgent.browser.isIE = goog.labs.userAgent.browser.matchIE_;\n\n\n/**\n * @return {boolean} Whether the user's browser is Edge.\n */\ngoog.labs.userAgent.browser.isEdge = goog.labs.userAgent.browser.matchEdge_;\n\n\n/**\n * @return {boolean} Whether the user's browser is Firefox.\n */\ngoog.labs.userAgent.browser.isFirefox =\n    goog.labs.userAgent.browser.matchFirefox_;\n\n\n/**\n * @return {boolean} Whether the user's browser is Safari.\n */\ngoog.labs.userAgent.browser.isSafari = goog.labs.userAgent.browser.matchSafari_;\n\n\n/**\n * @return {boolean} Whether the user's browser is Coast (Opera's Webkit-based\n *     iOS browser).\n */\ngoog.labs.userAgent.browser.isCoast = goog.labs.userAgent.browser.matchCoast_;\n\n\n/**\n * @return {boolean} Whether the user's browser is iOS Webview.\n */\ngoog.labs.userAgent.browser.isIosWebview =\n    goog.labs.userAgent.browser.matchIosWebview_;\n\n\n/**\n * @return {boolean} Whether the user's browser is Chrome.\n */\ngoog.labs.userAgent.browser.isChrome = goog.labs.userAgent.browser.matchChrome_;\n\n\n/**\n * @return {boolean} Whether the user's browser is the Android browser.\n */\ngoog.labs.userAgent.browser.isAndroidBrowser =\n    goog.labs.userAgent.browser.matchAndroidBrowser_;\n\n\n/**\n * For more information, see:\n * http://docs.aws.amazon.com/silk/latest/developerguide/user-agent.html\n * @return {boolean} Whether the user's browser is Silk.\n */\ngoog.labs.userAgent.browser.isSilk = function() {\n  return goog.labs.userAgent.util.matchUserAgent('Silk');\n};\n\n\n/**\n * @return {string} The browser version or empty string if version cannot be\n *     determined. Note that for Internet Explorer, this returns the version of\n *     the browser, not the version of the rendering engine. (IE 8 in\n *     compatibility mode will return 8.0 rather than 7.0. To determine the\n *     rendering engine version, look at document.documentMode instead. See\n *     http://msdn.microsoft.com/en-us/library/cc196988(v=vs.85).aspx for more\n *     details.)\n */\ngoog.labs.userAgent.browser.getVersion = function() {\n  var userAgentString = goog.labs.userAgent.util.getUserAgent();\n  // Special case IE since IE's version is inside the parenthesis and\n  // without the '/'.\n  if (goog.labs.userAgent.browser.isIE()) {\n    return goog.labs.userAgent.browser.getIEVersion_(userAgentString);\n  }\n\n  var versionTuples =\n      goog.labs.userAgent.util.extractVersionTuples(userAgentString);\n\n  // Construct a map for easy lookup.\n  var versionMap = {};\n  goog.array.forEach(versionTuples, function(tuple) {\n    // Note that the tuple is of length three, but we only care about the\n    // first two.\n    var key = tuple[0];\n    var value = tuple[1];\n    versionMap[key] = value;\n  });\n\n  var versionMapHasKey = goog.partial(goog.object.containsKey, versionMap);\n\n  // Gives the value with the first key it finds, otherwise empty string.\n  function lookUpValueWithKeys(keys) {\n    var key = goog.array.find(keys, versionMapHasKey);\n    return versionMap[key] || '';\n  }\n\n  // Check Opera before Chrome since Opera 15+ has \"Chrome\" in the string.\n  // See\n  // http://my.opera.com/ODIN/blog/2013/07/15/opera-user-agent-strings-opera-15-and-beyond\n  if (goog.labs.userAgent.browser.isOpera()) {\n    // Opera 10 has Version/10.0 but Opera/9.8, so look for \"Version\" first.\n    // Opera uses 'OPR' for more recent UAs.\n    return lookUpValueWithKeys(['Version', 'Opera']);\n  }\n\n  // Check Edge before Chrome since it has Chrome in the string.\n  if (goog.labs.userAgent.browser.isEdge()) {\n    return lookUpValueWithKeys(['Edge']);\n  }\n\n  if (goog.labs.userAgent.browser.isChrome()) {\n    return lookUpValueWithKeys(['Chrome', 'CriOS']);\n  }\n\n  // Usually products browser versions are in the third tuple after \"Mozilla\"\n  // and the engine.\n  var tuple = versionTuples[2];\n  return tuple && tuple[1] || '';\n};\n\n\n/**\n * @param {string|number} version The version to check.\n * @return {boolean} Whether the browser version is higher or the same as the\n *     given version.\n */\ngoog.labs.userAgent.browser.isVersionOrHigher = function(version) {\n  return goog.string.internal.compareVersions(\n             goog.labs.userAgent.browser.getVersion(), version) >= 0;\n};\n\n\n/**\n * Determines IE version. More information:\n * http://msdn.microsoft.com/en-us/library/ie/bg182625(v=vs.85).aspx#uaString\n * http://msdn.microsoft.com/en-us/library/hh869301(v=vs.85).aspx\n * http://blogs.msdn.com/b/ie/archive/2010/03/23/introducing-ie9-s-user-agent-string.aspx\n * http://blogs.msdn.com/b/ie/archive/2009/01/09/the-internet-explorer-8-user-agent-string-updated-edition.aspx\n *\n * @param {string} userAgent the User-Agent.\n * @return {string}\n * @private\n */\ngoog.labs.userAgent.browser.getIEVersion_ = function(userAgent) {\n  // IE11 may identify itself as MSIE 9.0 or MSIE 10.0 due to an IE 11 upgrade\n  // bug. Example UA:\n  // Mozilla/5.0 (MSIE 9.0; Windows NT 6.1; WOW64; Trident/7.0; rv:11.0)\n  // like Gecko.\n  // See http://www.whatismybrowser.com/developers/unknown-user-agent-fragments.\n  var rv = /rv: *([\\d\\.]*)/.exec(userAgent);\n  if (rv && rv[1]) {\n    return rv[1];\n  }\n\n  var version = '';\n  var msie = /MSIE +([\\d\\.]+)/.exec(userAgent);\n  if (msie && msie[1]) {\n    // IE in compatibility mode usually identifies itself as MSIE 7.0; in this\n    // case, use the Trident version to determine the version of IE. For more\n    // details, see the links above.\n    var tridentVersion = /Trident\\/(\\d.\\d)/.exec(userAgent);\n    if (msie[1] == '7.0') {\n      if (tridentVersion && tridentVersion[1]) {\n        switch (tridentVersion[1]) {\n          case '4.0':\n            version = '8.0';\n            break;\n          case '5.0':\n            version = '9.0';\n            break;\n          case '6.0':\n            version = '10.0';\n            break;\n          case '7.0':\n            version = '11.0';\n            break;\n        }\n      } else {\n        version = '7.0';\n      }\n    } else {\n      version = msie[1];\n    }\n  }\n  return version;\n};\n","~:compiled-at",1572373717128,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.labs.useragent.browser.js\",\n\"lineCount\":189,\n\"mappings\":\"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\",\n\"sources\":[\"goog/labs/useragent/browser.js\"],\n\"sourcesContent\":[\"// Copyright 2013 The Closure Library Authors. All Rights Reserved.\\n//\\n// Licensed under the Apache License, Version 2.0 (the \\\"License\\\");\\n// you may not use this file except in compliance with the License.\\n// You may obtain a copy of the License at\\n//\\n//      http://www.apache.org/licenses/LICENSE-2.0\\n//\\n// Unless required by applicable law or agreed to in writing, software\\n// distributed under the License is distributed on an \\\"AS-IS\\\" BASIS,\\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\\n// See the License for the specific language governing permissions and\\n// limitations under the License.\\n\\n/**\\n * @fileoverview Closure user agent detection (Browser).\\n * @see <a href=\\\"http://www.useragentstring.com/\\\">User agent strings</a>\\n * For more information on rendering engine, platform, or device see the other\\n * sub-namespaces in goog.labs.userAgent, goog.labs.userAgent.platform,\\n * goog.labs.userAgent.device respectively.)\\n *\\n * @author martone@google.com (Andy Martone)\\n */\\n\\ngoog.provide('goog.labs.userAgent.browser');\\n\\ngoog.require('goog.array');\\ngoog.require('goog.labs.userAgent.util');\\ngoog.require('goog.object');\\ngoog.require('goog.string.internal');\\n\\n\\n// TODO(nnaze): Refactor to remove excessive exclusion logic in matching\\n// functions.\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Opera.  Note: Chromium\\n *     based Opera (Opera 15+) is detected as Chrome to avoid unnecessary\\n *     special casing.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchOpera_ = function() {\\n  return goog.labs.userAgent.util.matchUserAgent('Opera');\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is IE.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchIE_ = function() {\\n  return goog.labs.userAgent.util.matchUserAgent('Trident') ||\\n      goog.labs.userAgent.util.matchUserAgent('MSIE');\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Edge.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchEdge_ = function() {\\n  return goog.labs.userAgent.util.matchUserAgent('Edge');\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Firefox.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchFirefox_ = function() {\\n  return goog.labs.userAgent.util.matchUserAgent('Firefox') ||\\n      goog.labs.userAgent.util.matchUserAgent('FxiOS');\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Safari.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchSafari_ = function() {\\n  return goog.labs.userAgent.util.matchUserAgent('Safari') &&\\n      !(goog.labs.userAgent.browser.matchChrome_() ||\\n        goog.labs.userAgent.browser.matchCoast_() ||\\n        goog.labs.userAgent.browser.matchOpera_() ||\\n        goog.labs.userAgent.browser.matchEdge_() ||\\n        goog.labs.userAgent.browser.matchFirefox_() ||\\n        goog.labs.userAgent.browser.isSilk() ||\\n        goog.labs.userAgent.util.matchUserAgent('Android'));\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Coast (Opera's Webkit-based\\n *     iOS browser).\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchCoast_ = function() {\\n  return goog.labs.userAgent.util.matchUserAgent('Coast');\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is iOS Webview.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchIosWebview_ = function() {\\n  // iOS Webview does not show up as Chrome or Safari. Also check for Opera's\\n  // WebKit-based iOS browser, Coast.\\n  return (goog.labs.userAgent.util.matchUserAgent('iPad') ||\\n          goog.labs.userAgent.util.matchUserAgent('iPhone')) &&\\n      !goog.labs.userAgent.browser.matchSafari_() &&\\n      !goog.labs.userAgent.browser.matchChrome_() &&\\n      !goog.labs.userAgent.browser.matchCoast_() &&\\n      !goog.labs.userAgent.browser.matchFirefox_() &&\\n      goog.labs.userAgent.util.matchUserAgent('AppleWebKit');\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Chrome.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchChrome_ = function() {\\n  return (goog.labs.userAgent.util.matchUserAgent('Chrome') ||\\n          goog.labs.userAgent.util.matchUserAgent('CriOS')) &&\\n      !goog.labs.userAgent.browser.matchEdge_();\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is the Android browser.\\n * @private\\n */\\ngoog.labs.userAgent.browser.matchAndroidBrowser_ = function() {\\n  // Android can appear in the user agent string for Chrome on Android.\\n  // This is not the Android standalone browser if it does.\\n  return goog.labs.userAgent.util.matchUserAgent('Android') &&\\n      !(goog.labs.userAgent.browser.isChrome() ||\\n        goog.labs.userAgent.browser.isFirefox() ||\\n        goog.labs.userAgent.browser.isOpera() ||\\n        goog.labs.userAgent.browser.isSilk());\\n};\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Opera.\\n */\\ngoog.labs.userAgent.browser.isOpera = goog.labs.userAgent.browser.matchOpera_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is IE.\\n */\\ngoog.labs.userAgent.browser.isIE = goog.labs.userAgent.browser.matchIE_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Edge.\\n */\\ngoog.labs.userAgent.browser.isEdge = goog.labs.userAgent.browser.matchEdge_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Firefox.\\n */\\ngoog.labs.userAgent.browser.isFirefox =\\n    goog.labs.userAgent.browser.matchFirefox_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Safari.\\n */\\ngoog.labs.userAgent.browser.isSafari = goog.labs.userAgent.browser.matchSafari_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Coast (Opera's Webkit-based\\n *     iOS browser).\\n */\\ngoog.labs.userAgent.browser.isCoast = goog.labs.userAgent.browser.matchCoast_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is iOS Webview.\\n */\\ngoog.labs.userAgent.browser.isIosWebview =\\n    goog.labs.userAgent.browser.matchIosWebview_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is Chrome.\\n */\\ngoog.labs.userAgent.browser.isChrome = goog.labs.userAgent.browser.matchChrome_;\\n\\n\\n/**\\n * @return {boolean} Whether the user's browser is the Android browser.\\n */\\ngoog.labs.userAgent.browser.isAndroidBrowser =\\n    goog.labs.userAgent.browser.matchAndroidBrowser_;\\n\\n\\n/**\\n * For more information, see:\\n * http://docs.aws.amazon.com/silk/latest/developerguide/user-agent.html\\n * @return {boolean} Whether the user's browser is Silk.\\n */\\ngoog.labs.userAgent.browser.isSilk = function() {\\n  return goog.labs.userAgent.util.matchUserAgent('Silk');\\n};\\n\\n\\n/**\\n * @return {string} The browser version or empty string if version cannot be\\n *     determined. Note that for Internet Explorer, this returns the version of\\n *     the browser, not the version of the rendering engine. (IE 8 in\\n *     compatibility mode will return 8.0 rather than 7.0. To determine the\\n *     rendering engine version, look at document.documentMode instead. See\\n *     http://msdn.microsoft.com/en-us/library/cc196988(v=vs.85).aspx for more\\n *     details.)\\n */\\ngoog.labs.userAgent.browser.getVersion = function() {\\n  var userAgentString = goog.labs.userAgent.util.getUserAgent();\\n  // Special case IE since IE's version is inside the parenthesis and\\n  // without the '/'.\\n  if (goog.labs.userAgent.browser.isIE()) {\\n    return goog.labs.userAgent.browser.getIEVersion_(userAgentString);\\n  }\\n\\n  var versionTuples =\\n      goog.labs.userAgent.util.extractVersionTuples(userAgentString);\\n\\n  // Construct a map for easy lookup.\\n  var versionMap = {};\\n  goog.array.forEach(versionTuples, function(tuple) {\\n    // Note that the tuple is of length three, but we only care about the\\n    // first two.\\n    var key = tuple[0];\\n    var value = tuple[1];\\n    versionMap[key] = value;\\n  });\\n\\n  var versionMapHasKey = goog.partial(goog.object.containsKey, versionMap);\\n\\n  // Gives the value with the first key it finds, otherwise empty string.\\n  function lookUpValueWithKeys(keys) {\\n    var key = goog.array.find(keys, versionMapHasKey);\\n    return versionMap[key] || '';\\n  }\\n\\n  // Check Opera before Chrome since Opera 15+ has \\\"Chrome\\\" in the string.\\n  // See\\n  // http://my.opera.com/ODIN/blog/2013/07/15/opera-user-agent-strings-opera-15-and-beyond\\n  if (goog.labs.userAgent.browser.isOpera()) {\\n    // Opera 10 has Version/10.0 but Opera/9.8, so look for \\\"Version\\\" first.\\n    // Opera uses 'OPR' for more recent UAs.\\n    return lookUpValueWithKeys(['Version', 'Opera']);\\n  }\\n\\n  // Check Edge before Chrome since it has Chrome in the string.\\n  if (goog.labs.userAgent.browser.isEdge()) {\\n    return lookUpValueWithKeys(['Edge']);\\n  }\\n\\n  if (goog.labs.userAgent.browser.isChrome()) {\\n    return lookUpValueWithKeys(['Chrome', 'CriOS']);\\n  }\\n\\n  // Usually products browser versions are in the third tuple after \\\"Mozilla\\\"\\n  // and the engine.\\n  var tuple = versionTuples[2];\\n  return tuple && tuple[1] || '';\\n};\\n\\n\\n/**\\n * @param {string|number} version The version to check.\\n * @return {boolean} Whether the browser version is higher or the same as the\\n *     given version.\\n */\\ngoog.labs.userAgent.browser.isVersionOrHigher = function(version) {\\n  return goog.string.internal.compareVersions(\\n             goog.labs.userAgent.browser.getVersion(), version) >= 0;\\n};\\n\\n\\n/**\\n * Determines IE version. More information:\\n * http://msdn.microsoft.com/en-us/library/ie/bg182625(v=vs.85).aspx#uaString\\n * http://msdn.microsoft.com/en-us/library/hh869301(v=vs.85).aspx\\n * http://blogs.msdn.com/b/ie/archive/2010/03/23/introducing-ie9-s-user-agent-string.aspx\\n * http://blogs.msdn.com/b/ie/archive/2009/01/09/the-internet-explorer-8-user-agent-string-updated-edition.aspx\\n *\\n * @param {string} userAgent the User-Agent.\\n * @return {string}\\n * @private\\n */\\ngoog.labs.userAgent.browser.getIEVersion_ = function(userAgent) {\\n  // IE11 may identify itself as MSIE 9.0 or MSIE 10.0 due to an IE 11 upgrade\\n  // bug. Example UA:\\n  // Mozilla/5.0 (MSIE 9.0; Windows NT 6.1; WOW64; Trident/7.0; rv:11.0)\\n  // like Gecko.\\n  // See http://www.whatismybrowser.com/developers/unknown-user-agent-fragments.\\n  var rv = /rv: *([\\\\d\\\\.]*)/.exec(userAgent);\\n  if (rv && rv[1]) {\\n    return rv[1];\\n  }\\n\\n  var version = '';\\n  var msie = /MSIE +([\\\\d\\\\.]+)/.exec(userAgent);\\n  if (msie && msie[1]) {\\n    // IE in compatibility mode usually identifies itself as MSIE 7.0; in this\\n    // case, use the Trident version to determine the version of IE. For more\\n    // details, see the links above.\\n    var tridentVersion = /Trident\\\\/(\\\\d.\\\\d)/.exec(userAgent);\\n    if (msie[1] == '7.0') {\\n      if (tridentVersion && tridentVersion[1]) {\\n        switch (tridentVersion[1]) {\\n          case '4.0':\\n            version = '8.0';\\n            break;\\n          case '5.0':\\n            version = '9.0';\\n            break;\\n          case '6.0':\\n            version = '10.0';\\n            break;\\n          case '7.0':\\n            version = '11.0';\\n            break;\\n        }\\n      } else {\\n        version = '7.0';\\n      }\\n    } else {\\n      version = msie[1];\\n    }\\n  }\\n  return version;\\n};\\n\"],\n\"names\":[\"goog\",\"provide\",\"require\",\"labs\",\"userAgent\",\"browser\",\"matchOpera_\",\"goog.labs.userAgent.browser.matchOpera_\",\"util\",\"matchUserAgent\",\"matchIE_\",\"goog.labs.userAgent.browser.matchIE_\",\"matchEdge_\",\"goog.labs.userAgent.browser.matchEdge_\",\"matchFirefox_\",\"goog.labs.userAgent.browser.matchFirefox_\",\"matchSafari_\",\"goog.labs.userAgent.browser.matchSafari_\",\"matchChrome_\",\"matchCoast_\",\"isSilk\",\"goog.labs.userAgent.browser.matchCoast_\",\"matchIosWebview_\",\"goog.labs.userAgent.browser.matchIosWebview_\",\"goog.labs.userAgent.browser.matchChrome_\",\"matchAndroidBrowser_\",\"goog.labs.userAgent.browser.matchAndroidBrowser_\",\"isChrome\",\"isFirefox\",\"isOpera\",\"isIE\",\"isEdge\",\"isSafari\",\"isCoast\",\"isIosWebview\",\"isAndroidBrowser\",\"goog.labs.userAgent.browser.isSilk\",\"getVersion\",\"goog.labs.userAgent.browser.getVersion\",\"userAgentString\",\"getUserAgent\",\"getIEVersion_\",\"versionTuples\",\"extractVersionTuples\",\"versionMap\",\"array\",\"forEach\",\"tuple\",\"key\",\"value\",\"versionMapHasKey\",\"partial\",\"object\",\"containsKey\",\"lookUpValueWithKeys\",\"keys\",\"find\",\"isVersionOrHigher\",\"goog.labs.userAgent.browser.isVersionOrHigher\",\"version\",\"string\",\"internal\",\"compareVersions\",\"goog.labs.userAgent.browser.getIEVersion_\",\"rv\",\"exec\",\"msie\",\"tridentVersion\"]\n}\n"]