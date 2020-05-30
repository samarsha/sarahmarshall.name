// The KaTeX options.
const options = displayMode => ({
  displayMode: displayMode,
  throwOnError: false,
  macros: {
    "\\bra": "\\mathinner{\\langle{#1}|}",
    "\\ket": "\\mathinner{|{#1}\\rangle}",
    "\\braket": "\\mathinner{\\langle{#1}\\rangle}",
    "\\Bra": "\\left\\langle#1\\right|",
    "\\Ket": "\\left|#1\\right\\rangle"
  }
});

// Renders an element.
const render = displayMode => element =>
  katex.render(element.textContent, element, options(displayMode));

document.querySelectorAll(".math.inline").forEach(render(false));
document.querySelectorAll(".math.display").forEach(render(true));
