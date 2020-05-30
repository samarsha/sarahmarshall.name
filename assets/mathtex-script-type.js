function options(displayMode) {
  return {
    displayMode: displayMode,
    throwOnError: false,
    macros: {
      "\\bra": "\\mathinner{\\langle{#1}|}",
      "\\ket": "\\mathinner{|{#1}\\rangle}",
      "\\braket": "\\mathinner{\\langle{#1}\\rangle}",
      "\\Bra": "\\left\\langle#1\\right|",
      "\\Ket": "\\left|#1\\right\\rangle"
    }
  };
}

function equation(text, options) {
  const element = document.createElement(options.displayMode ? "div" : "span");
  element.setAttribute("class", options.displayMode ? "equation" : "inline-equation");
  katex.render(text, element, options);
  return element;
}

const scripts = document.querySelectorAll("script[type='math/tex'], script[type^='math/tex;']");
for (const script of scripts) {
  const displayMode = script.type.split(";").map(s => s.trim()).includes("mode=display");
  script.parentNode.replaceChild(equation(script.text, options(displayMode)), script);
}
