["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/dom/asserts.js"],"~:js","goog.provide(\"goog.dom.asserts\");\ngoog.require(\"goog.asserts\");\n/**\n * @param {?Object} o\n * @return {!Location}\n */\ngoog.dom.asserts.assertIsLocation = function(o) {\n  if (goog.asserts.ENABLE_ASSERTS) {\n    var win = goog.dom.asserts.getWindow_(o);\n    if (typeof win.Location != \"undefined\" && typeof win.Element != \"undefined\") {\n      goog.asserts.assert(o && (o instanceof win.Location || !(o instanceof win.Element)), \"Argument is not a Location (or a non-Element mock); got: %s\", goog.dom.asserts.debugStringForType_(o));\n    }\n  }\n  return (/** @type {!Location} */ (o));\n};\n/**\n * @private\n * @param {?Object} o\n * @param {string} typename\n * @return {!Element}\n */\ngoog.dom.asserts.assertIsElementType_ = function(o, typename) {\n  if (goog.asserts.ENABLE_ASSERTS) {\n    var win = goog.dom.asserts.getWindow_(o);\n    if (typeof win[typename] != \"undefined\" && typeof win.Location != \"undefined\" && typeof win.Element != \"undefined\") {\n      goog.asserts.assert(o && (o instanceof win[typename] || !(o instanceof win.Location || o instanceof win.Element)), \"Argument is not a %s (or a non-Element, non-Location mock); got: %s\", typename, goog.dom.asserts.debugStringForType_(o));\n    }\n  }\n  return (/** @type {!Element} */ (o));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLAnchorElement}\n */\ngoog.dom.asserts.assertIsHTMLAnchorElement = function(o) {\n  return (/** @type {!HTMLAnchorElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLAnchorElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLButtonElement}\n */\ngoog.dom.asserts.assertIsHTMLButtonElement = function(o) {\n  return (/** @type {!HTMLButtonElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLButtonElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLLinkElement}\n */\ngoog.dom.asserts.assertIsHTMLLinkElement = function(o) {\n  return (/** @type {!HTMLLinkElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLLinkElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLImageElement}\n */\ngoog.dom.asserts.assertIsHTMLImageElement = function(o) {\n  return (/** @type {!HTMLImageElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLImageElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLAudioElement}\n */\ngoog.dom.asserts.assertIsHTMLAudioElement = function(o) {\n  return (/** @type {!HTMLAudioElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLAudioElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLVideoElement}\n */\ngoog.dom.asserts.assertIsHTMLVideoElement = function(o) {\n  return (/** @type {!HTMLVideoElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLVideoElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLInputElement}\n */\ngoog.dom.asserts.assertIsHTMLInputElement = function(o) {\n  return (/** @type {!HTMLInputElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLInputElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLTextAreaElement}\n */\ngoog.dom.asserts.assertIsHTMLTextAreaElement = function(o) {\n  return (/** @type {!HTMLTextAreaElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLTextAreaElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLCanvasElement}\n */\ngoog.dom.asserts.assertIsHTMLCanvasElement = function(o) {\n  return (/** @type {!HTMLCanvasElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLCanvasElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLEmbedElement}\n */\ngoog.dom.asserts.assertIsHTMLEmbedElement = function(o) {\n  return (/** @type {!HTMLEmbedElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLEmbedElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLFormElement}\n */\ngoog.dom.asserts.assertIsHTMLFormElement = function(o) {\n  return (/** @type {!HTMLFormElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLFormElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLFrameElement}\n */\ngoog.dom.asserts.assertIsHTMLFrameElement = function(o) {\n  return (/** @type {!HTMLFrameElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLFrameElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLIFrameElement}\n */\ngoog.dom.asserts.assertIsHTMLIFrameElement = function(o) {\n  return (/** @type {!HTMLIFrameElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLIFrameElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLObjectElement}\n */\ngoog.dom.asserts.assertIsHTMLObjectElement = function(o) {\n  return (/** @type {!HTMLObjectElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLObjectElement\")));\n};\n/**\n * @param {?Object} o\n * @return {!HTMLScriptElement}\n */\ngoog.dom.asserts.assertIsHTMLScriptElement = function(o) {\n  return (/** @type {!HTMLScriptElement} */ (goog.dom.asserts.assertIsElementType_(o, \"HTMLScriptElement\")));\n};\n/**\n * @private\n * @param {*} value\n * @return {string}\n */\ngoog.dom.asserts.debugStringForType_ = function(value) {\n  if (goog.isObject(value)) {\n    return value.constructor.displayName || value.constructor.name || Object.prototype.toString.call(value);\n  } else {\n    return value === undefined ? \"undefined\" : value === null ? \"null\" : typeof value;\n  }\n};\n/**\n * @private\n * @param {?Object} o\n * @return {!Window}\n * @suppress {strictMissingProperties}\n */\ngoog.dom.asserts.getWindow_ = function(o) {\n  var doc = o && o.ownerDocument;\n  var win = doc && (/** @type {?Window} */ (doc.defaultView || doc.parentWindow));\n  return win || /** @type {!Window} */ (goog.global);\n};\n","~:source","// Copyright 2017 The Closure Library Authors. All Rights Reserved.\n//\n// Licensed under the Apache License, Version 2.0 (the \"License\");\n// you may not use this file except in compliance with the License.\n// You may obtain a copy of the License at\n//\n//      http://www.apache.org/licenses/LICENSE-2.0\n//\n// Unless required by applicable law or agreed to in writing, software\n// distributed under the License is distributed on an \"AS-IS\" BASIS,\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n// See the License for the specific language governing permissions and\n// limitations under the License.\n\ngoog.provide('goog.dom.asserts');\n\ngoog.require('goog.asserts');\n\n/**\n * @fileoverview Custom assertions to ensure that an element has the appropriate\n * type.\n *\n * Using a goog.dom.safe wrapper on an object on the incorrect type (via an\n * incorrect static type cast) can result in security bugs: For instance,\n * g.d.s.setAnchorHref ensures that the URL assigned to the .href attribute\n * satisfies the SafeUrl contract, i.e., is safe to dereference as a hyperlink.\n * However, the value assigned to a HTMLLinkElement's .href property requires\n * the stronger TrustedResourceUrl contract, since it can refer to a stylesheet.\n * Thus, using g.d.s.setAnchorHref on an (incorrectly statically typed) object\n * of type HTMLLinkElement can result in a security vulnerability.\n * Assertions of the correct run-time type help prevent such incorrect use.\n *\n * In some cases, code using the DOM API is tested using mock objects (e.g., a\n * plain object such as {'href': url} instead of an actual Location object).\n * To allow such mocking, the assertions permit objects of types that are not\n * relevant DOM API objects at all (for instance, not Element or Location).\n *\n * Note that instanceof checks don't work straightforwardly in older versions of\n * IE, or across frames (see,\n * http://stackoverflow.com/questions/384286/javascript-isdom-how-do-you-check-if-a-javascript-object-is-a-dom-object,\n * http://stackoverflow.com/questions/26248599/instanceof-htmlelement-in-iframe-is-not-element-or-object).\n *\n * Hence, these assertions may pass vacuously in such scenarios. The resulting\n * risk of security bugs is limited by the following factors:\n *  - A bug can only arise in scenarios involving incorrect static typing (the\n *    wrapper methods are statically typed to demand objects of the appropriate,\n *    precise type).\n *  - Typically, code is tested and exercised in multiple browsers.\n */\n\n/**\n * Asserts that a given object is a Location.\n *\n * To permit this assertion to pass in the context of tests where DOM APIs might\n * be mocked, also accepts any other type except for subtypes of {!Element}.\n * This is to ensure that, for instance, HTMLLinkElement is not being used in\n * place of a Location, since this could result in security bugs due to stronger\n * contracts required for assignments to the href property of the latter.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!Location}\n */\ngoog.dom.asserts.assertIsLocation = function(o) {\n  if (goog.asserts.ENABLE_ASSERTS) {\n    var win = goog.dom.asserts.getWindow_(o);\n    if (typeof win.Location != 'undefined' &&\n        typeof win.Element != 'undefined') {\n      goog.asserts.assert(\n          o && (o instanceof win.Location || !(o instanceof win.Element)),\n          'Argument is not a Location (or a non-Element mock); got: %s',\n          goog.dom.asserts.debugStringForType_(o));\n    }\n  }\n  return /** @type {!Location} */ (o);\n};\n\n\n/**\n * Asserts that a given object is either the given subtype of Element\n * or a non-Element, non-Location Mock.\n *\n * To permit this assertion to pass in the context of tests where DOM\n * APIs might be mocked, also accepts any other type except for\n * subtypes of {!Element}.  This is to ensure that, for instance,\n * HTMLScriptElement is not being used in place of a HTMLImageElement,\n * since this could result in security bugs due to stronger contracts\n * required for assignments to the src property of the latter.\n *\n * The DOM type is looked up in the window the object belongs to.  In\n * some contexts, this might not be possible (e.g. when running tests\n * outside a browser, cross-domain lookup). In this case, the\n * assertions are skipped.\n *\n * @param {?Object} o The object whose type to assert.\n * @param {string} typename The name of the DOM type.\n * @return {!Element} The object.\n * @private\n */\n// TODO(bangert): Make an analog of goog.dom.TagName to correctly handle casts?\ngoog.dom.asserts.assertIsElementType_ = function(o, typename) {\n  if (goog.asserts.ENABLE_ASSERTS) {\n    var win = goog.dom.asserts.getWindow_(o);\n    if (typeof win[typename] != 'undefined' &&\n        typeof win.Location != 'undefined' &&\n        typeof win.Element != 'undefined') {\n      goog.asserts.assert(\n          o &&\n              (o instanceof win[typename] ||\n               !((o instanceof win.Location) || (o instanceof win.Element))),\n          'Argument is not a %s (or a non-Element, non-Location mock); got: %s',\n          typename, goog.dom.asserts.debugStringForType_(o));\n    }\n  }\n  return /** @type {!Element} */ (o);\n};\n\n/**\n * Asserts that a given object is a HTMLAnchorElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not of type Location nor a subtype\n * of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLAnchorElement}\n */\ngoog.dom.asserts.assertIsHTMLAnchorElement = function(o) {\n  return /** @type {!HTMLAnchorElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLAnchorElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLButtonElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLButtonElement}\n */\ngoog.dom.asserts.assertIsHTMLButtonElement = function(o) {\n  return /** @type {!HTMLButtonElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLButtonElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLLinkElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLLinkElement}\n */\ngoog.dom.asserts.assertIsHTMLLinkElement = function(o) {\n  return /** @type {!HTMLLinkElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLLinkElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLImageElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLImageElement}\n */\ngoog.dom.asserts.assertIsHTMLImageElement = function(o) {\n  return /** @type {!HTMLImageElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLImageElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLAudioElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLAudioElement}\n */\ngoog.dom.asserts.assertIsHTMLAudioElement = function(o) {\n  return /** @type {!HTMLAudioElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLAudioElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLVideoElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLVideoElement}\n */\ngoog.dom.asserts.assertIsHTMLVideoElement = function(o) {\n  return /** @type {!HTMLVideoElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLVideoElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLInputElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLInputElement}\n */\ngoog.dom.asserts.assertIsHTMLInputElement = function(o) {\n  return /** @type {!HTMLInputElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLInputElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLTextAreaElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLTextAreaElement}\n */\ngoog.dom.asserts.assertIsHTMLTextAreaElement = function(o) {\n  return /** @type {!HTMLTextAreaElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLTextAreaElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLCanvasElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLCanvasElement}\n */\ngoog.dom.asserts.assertIsHTMLCanvasElement = function(o) {\n  return /** @type {!HTMLCanvasElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLCanvasElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLEmbedElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLEmbedElement}\n */\ngoog.dom.asserts.assertIsHTMLEmbedElement = function(o) {\n  return /** @type {!HTMLEmbedElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLEmbedElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLFormElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLFormElement}\n */\ngoog.dom.asserts.assertIsHTMLFormElement = function(o) {\n  return /** @type {!HTMLFormElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLFormElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLFrameElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLFrameElement}\n */\ngoog.dom.asserts.assertIsHTMLFrameElement = function(o) {\n  return /** @type {!HTMLFrameElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLFrameElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLIFrameElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLIFrameElement}\n */\ngoog.dom.asserts.assertIsHTMLIFrameElement = function(o) {\n  return /** @type {!HTMLIFrameElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLIFrameElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLObjectElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLObjectElement}\n */\ngoog.dom.asserts.assertIsHTMLObjectElement = function(o) {\n  return /** @type {!HTMLObjectElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLObjectElement'));\n};\n\n/**\n * Asserts that a given object is a HTMLScriptElement.\n *\n * To permit this assertion to pass in the context of tests where elements might\n * be mocked, also accepts objects that are not a subtype of Element.\n *\n * @param {?Object} o The object whose type to assert.\n * @return {!HTMLScriptElement}\n */\ngoog.dom.asserts.assertIsHTMLScriptElement = function(o) {\n  return /** @type {!HTMLScriptElement} */ (\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLScriptElement'));\n};\n\n/**\n * Returns a string representation of a value's type.\n *\n * @param {*} value An object, or primitive.\n * @return {string} The best display name for the value.\n * @private\n */\ngoog.dom.asserts.debugStringForType_ = function(value) {\n  if (goog.isObject(value)) {\n    return value.constructor.displayName || value.constructor.name ||\n        Object.prototype.toString.call(value);\n  } else {\n    return value === undefined ? 'undefined' :\n                                 value === null ? 'null' : typeof value;\n  }\n};\n\n/**\n * Gets window of element.\n * @param {?Object} o\n * @return {!Window}\n * @private\n * @suppress {strictMissingProperties} ownerDocument not defined on Object\n */\ngoog.dom.asserts.getWindow_ = function(o) {\n  var doc = o && o.ownerDocument;\n  var win = doc && /** @type {?Window} */ (doc.defaultView || doc.parentWindow);\n  return win || /** @type {!Window} */ (goog.global);\n};\n","~:compiled-at",1572373717145,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.dom.asserts.js\",\n\"lineCount\":159,\n\"mappings\":\"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\",\n\"sources\":[\"goog/dom/asserts.js\"],\n\"sourcesContent\":[\"// Copyright 2017 The Closure Library Authors. All Rights Reserved.\\n//\\n// Licensed under the Apache License, Version 2.0 (the \\\"License\\\");\\n// you may not use this file except in compliance with the License.\\n// You may obtain a copy of the License at\\n//\\n//      http://www.apache.org/licenses/LICENSE-2.0\\n//\\n// Unless required by applicable law or agreed to in writing, software\\n// distributed under the License is distributed on an \\\"AS-IS\\\" BASIS,\\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\\n// See the License for the specific language governing permissions and\\n// limitations under the License.\\n\\ngoog.provide('goog.dom.asserts');\\n\\ngoog.require('goog.asserts');\\n\\n/**\\n * @fileoverview Custom assertions to ensure that an element has the appropriate\\n * type.\\n *\\n * Using a goog.dom.safe wrapper on an object on the incorrect type (via an\\n * incorrect static type cast) can result in security bugs: For instance,\\n * g.d.s.setAnchorHref ensures that the URL assigned to the .href attribute\\n * satisfies the SafeUrl contract, i.e., is safe to dereference as a hyperlink.\\n * However, the value assigned to a HTMLLinkElement's .href property requires\\n * the stronger TrustedResourceUrl contract, since it can refer to a stylesheet.\\n * Thus, using g.d.s.setAnchorHref on an (incorrectly statically typed) object\\n * of type HTMLLinkElement can result in a security vulnerability.\\n * Assertions of the correct run-time type help prevent such incorrect use.\\n *\\n * In some cases, code using the DOM API is tested using mock objects (e.g., a\\n * plain object such as {'href': url} instead of an actual Location object).\\n * To allow such mocking, the assertions permit objects of types that are not\\n * relevant DOM API objects at all (for instance, not Element or Location).\\n *\\n * Note that instanceof checks don't work straightforwardly in older versions of\\n * IE, or across frames (see,\\n * http://stackoverflow.com/questions/384286/javascript-isdom-how-do-you-check-if-a-javascript-object-is-a-dom-object,\\n * http://stackoverflow.com/questions/26248599/instanceof-htmlelement-in-iframe-is-not-element-or-object).\\n *\\n * Hence, these assertions may pass vacuously in such scenarios. The resulting\\n * risk of security bugs is limited by the following factors:\\n *  - A bug can only arise in scenarios involving incorrect static typing (the\\n *    wrapper methods are statically typed to demand objects of the appropriate,\\n *    precise type).\\n *  - Typically, code is tested and exercised in multiple browsers.\\n */\\n\\n/**\\n * Asserts that a given object is a Location.\\n *\\n * To permit this assertion to pass in the context of tests where DOM APIs might\\n * be mocked, also accepts any other type except for subtypes of {!Element}.\\n * This is to ensure that, for instance, HTMLLinkElement is not being used in\\n * place of a Location, since this could result in security bugs due to stronger\\n * contracts required for assignments to the href property of the latter.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!Location}\\n */\\ngoog.dom.asserts.assertIsLocation = function(o) {\\n  if (goog.asserts.ENABLE_ASSERTS) {\\n    var win = goog.dom.asserts.getWindow_(o);\\n    if (typeof win.Location != 'undefined' &&\\n        typeof win.Element != 'undefined') {\\n      goog.asserts.assert(\\n          o && (o instanceof win.Location || !(o instanceof win.Element)),\\n          'Argument is not a Location (or a non-Element mock); got: %s',\\n          goog.dom.asserts.debugStringForType_(o));\\n    }\\n  }\\n  return /** @type {!Location} */ (o);\\n};\\n\\n\\n/**\\n * Asserts that a given object is either the given subtype of Element\\n * or a non-Element, non-Location Mock.\\n *\\n * To permit this assertion to pass in the context of tests where DOM\\n * APIs might be mocked, also accepts any other type except for\\n * subtypes of {!Element}.  This is to ensure that, for instance,\\n * HTMLScriptElement is not being used in place of a HTMLImageElement,\\n * since this could result in security bugs due to stronger contracts\\n * required for assignments to the src property of the latter.\\n *\\n * The DOM type is looked up in the window the object belongs to.  In\\n * some contexts, this might not be possible (e.g. when running tests\\n * outside a browser, cross-domain lookup). In this case, the\\n * assertions are skipped.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @param {string} typename The name of the DOM type.\\n * @return {!Element} The object.\\n * @private\\n */\\n// TODO(bangert): Make an analog of goog.dom.TagName to correctly handle casts?\\ngoog.dom.asserts.assertIsElementType_ = function(o, typename) {\\n  if (goog.asserts.ENABLE_ASSERTS) {\\n    var win = goog.dom.asserts.getWindow_(o);\\n    if (typeof win[typename] != 'undefined' &&\\n        typeof win.Location != 'undefined' &&\\n        typeof win.Element != 'undefined') {\\n      goog.asserts.assert(\\n          o &&\\n              (o instanceof win[typename] ||\\n               !((o instanceof win.Location) || (o instanceof win.Element))),\\n          'Argument is not a %s (or a non-Element, non-Location mock); got: %s',\\n          typename, goog.dom.asserts.debugStringForType_(o));\\n    }\\n  }\\n  return /** @type {!Element} */ (o);\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLAnchorElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not of type Location nor a subtype\\n * of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLAnchorElement}\\n */\\ngoog.dom.asserts.assertIsHTMLAnchorElement = function(o) {\\n  return /** @type {!HTMLAnchorElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLAnchorElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLButtonElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLButtonElement}\\n */\\ngoog.dom.asserts.assertIsHTMLButtonElement = function(o) {\\n  return /** @type {!HTMLButtonElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLButtonElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLLinkElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLLinkElement}\\n */\\ngoog.dom.asserts.assertIsHTMLLinkElement = function(o) {\\n  return /** @type {!HTMLLinkElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLLinkElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLImageElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLImageElement}\\n */\\ngoog.dom.asserts.assertIsHTMLImageElement = function(o) {\\n  return /** @type {!HTMLImageElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLImageElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLAudioElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLAudioElement}\\n */\\ngoog.dom.asserts.assertIsHTMLAudioElement = function(o) {\\n  return /** @type {!HTMLAudioElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLAudioElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLVideoElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLVideoElement}\\n */\\ngoog.dom.asserts.assertIsHTMLVideoElement = function(o) {\\n  return /** @type {!HTMLVideoElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLVideoElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLInputElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLInputElement}\\n */\\ngoog.dom.asserts.assertIsHTMLInputElement = function(o) {\\n  return /** @type {!HTMLInputElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLInputElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLTextAreaElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLTextAreaElement}\\n */\\ngoog.dom.asserts.assertIsHTMLTextAreaElement = function(o) {\\n  return /** @type {!HTMLTextAreaElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLTextAreaElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLCanvasElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLCanvasElement}\\n */\\ngoog.dom.asserts.assertIsHTMLCanvasElement = function(o) {\\n  return /** @type {!HTMLCanvasElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLCanvasElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLEmbedElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLEmbedElement}\\n */\\ngoog.dom.asserts.assertIsHTMLEmbedElement = function(o) {\\n  return /** @type {!HTMLEmbedElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLEmbedElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLFormElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLFormElement}\\n */\\ngoog.dom.asserts.assertIsHTMLFormElement = function(o) {\\n  return /** @type {!HTMLFormElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLFormElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLFrameElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLFrameElement}\\n */\\ngoog.dom.asserts.assertIsHTMLFrameElement = function(o) {\\n  return /** @type {!HTMLFrameElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLFrameElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLIFrameElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLIFrameElement}\\n */\\ngoog.dom.asserts.assertIsHTMLIFrameElement = function(o) {\\n  return /** @type {!HTMLIFrameElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLIFrameElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLObjectElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLObjectElement}\\n */\\ngoog.dom.asserts.assertIsHTMLObjectElement = function(o) {\\n  return /** @type {!HTMLObjectElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLObjectElement'));\\n};\\n\\n/**\\n * Asserts that a given object is a HTMLScriptElement.\\n *\\n * To permit this assertion to pass in the context of tests where elements might\\n * be mocked, also accepts objects that are not a subtype of Element.\\n *\\n * @param {?Object} o The object whose type to assert.\\n * @return {!HTMLScriptElement}\\n */\\ngoog.dom.asserts.assertIsHTMLScriptElement = function(o) {\\n  return /** @type {!HTMLScriptElement} */ (\\n      goog.dom.asserts.assertIsElementType_(o, 'HTMLScriptElement'));\\n};\\n\\n/**\\n * Returns a string representation of a value's type.\\n *\\n * @param {*} value An object, or primitive.\\n * @return {string} The best display name for the value.\\n * @private\\n */\\ngoog.dom.asserts.debugStringForType_ = function(value) {\\n  if (goog.isObject(value)) {\\n    return value.constructor.displayName || value.constructor.name ||\\n        Object.prototype.toString.call(value);\\n  } else {\\n    return value === undefined ? 'undefined' :\\n                                 value === null ? 'null' : typeof value;\\n  }\\n};\\n\\n/**\\n * Gets window of element.\\n * @param {?Object} o\\n * @return {!Window}\\n * @private\\n * @suppress {strictMissingProperties} ownerDocument not defined on Object\\n */\\ngoog.dom.asserts.getWindow_ = function(o) {\\n  var doc = o && o.ownerDocument;\\n  var win = doc && /** @type {?Window} */ (doc.defaultView || doc.parentWindow);\\n  return win || /** @type {!Window} */ (goog.global);\\n};\\n\"],\n\"names\":[\"goog\",\"provide\",\"require\",\"dom\",\"asserts\",\"assertIsLocation\",\"goog.dom.asserts.assertIsLocation\",\"o\",\"ENABLE_ASSERTS\",\"win\",\"getWindow_\",\"Location\",\"Element\",\"assert\",\"debugStringForType_\",\"assertIsElementType_\",\"goog.dom.asserts.assertIsElementType_\",\"typename\",\"assertIsHTMLAnchorElement\",\"goog.dom.asserts.assertIsHTMLAnchorElement\",\"assertIsHTMLButtonElement\",\"goog.dom.asserts.assertIsHTMLButtonElement\",\"assertIsHTMLLinkElement\",\"goog.dom.asserts.assertIsHTMLLinkElement\",\"assertIsHTMLImageElement\",\"goog.dom.asserts.assertIsHTMLImageElement\",\"assertIsHTMLAudioElement\",\"goog.dom.asserts.assertIsHTMLAudioElement\",\"assertIsHTMLVideoElement\",\"goog.dom.asserts.assertIsHTMLVideoElement\",\"assertIsHTMLInputElement\",\"goog.dom.asserts.assertIsHTMLInputElement\",\"assertIsHTMLTextAreaElement\",\"goog.dom.asserts.assertIsHTMLTextAreaElement\",\"assertIsHTMLCanvasElement\",\"goog.dom.asserts.assertIsHTMLCanvasElement\",\"assertIsHTMLEmbedElement\",\"goog.dom.asserts.assertIsHTMLEmbedElement\",\"assertIsHTMLFormElement\",\"goog.dom.asserts.assertIsHTMLFormElement\",\"assertIsHTMLFrameElement\",\"goog.dom.asserts.assertIsHTMLFrameElement\",\"assertIsHTMLIFrameElement\",\"goog.dom.asserts.assertIsHTMLIFrameElement\",\"assertIsHTMLObjectElement\",\"goog.dom.asserts.assertIsHTMLObjectElement\",\"assertIsHTMLScriptElement\",\"goog.dom.asserts.assertIsHTMLScriptElement\",\"goog.dom.asserts.debugStringForType_\",\"value\",\"isObject\",\"constructor\",\"displayName\",\"name\",\"Object\",\"prototype\",\"toString\",\"call\",\"undefined\",\"goog.dom.asserts.getWindow_\",\"doc\",\"ownerDocument\",\"defaultView\",\"parentWindow\",\"global\"]\n}\n"]