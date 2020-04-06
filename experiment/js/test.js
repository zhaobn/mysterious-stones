
document.getElementById("test-form").onchange = () => composeSelection("test-svg", "test-form");

function currentSelection (formId) {
  let selections = [];
  const inputs = document.getElementById(formId).elements;
  (Object.keys(inputs)).forEach((input) => {
    selections.push(inputs[input].value)
  });
  return selections.reverse().join(";")
}

function composeSelection (svgid, formid) {
  const selection = currentSelection(formid);
  if (selection.split(";")[0] === "--" || selection.split(";")[1] === "--") {
    console.log("Some configs are not selected")
  } else {
    let svg = document.getElementById(svgid);
    if (svg.childNodes.length > 0) {
      clearElement("test-stone")
    };
    svg = attachStone(svg, "test-stone", getOpts(selection));
  }
}
