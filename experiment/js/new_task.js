
/** Global variables */
const svgElements = [ "svg", "circle", "polygon", "rect" ];
const borderWidth = "8px";
const mar = 5;
const len = 80;

/** Configurations */
const colorDict = {
  "dark_1": '#6A1B9A',
  "dark_2": '#1565C0',
  "light_1": '#AB47BC',
  "light_2": '#64B5F6',
}
const allColors = Object.keys(colorDict);

const allShapes = [
  "circle",
  "p_3", // triangular
  "p_4", // square
  "p_5", // polygon 5 sides
  "p_6", // polygon 6 sides
  "p_7", // 7 sides
  "p_8", // 8 sides
]

const allStones = getStones(allColors, allShapes);
console.log(allStones);


/** Main body */
let t = createStone("test", "shape", 'ttt', {
  color: "black",
  cx: "40", cy: "40", r: "35",
  hasBorder: true,
});
let t2 = createStone("test", "shape", 'tt2', {
  color: "limegreen",
  points: calcPolygon({n:6,r:40,a:0}),
  hasBorder: false,
});

document.body.append(t)
document.body.append(t2)

/** Functions */
function getStones (colors, shapes) {
  let stones = []
  colors.forEach(c => {
    shapes.forEach(s => stones.push(c + ';' + s))
  })
  return(stones);
}

function sampleObj (objs) {
  return(objs[Math.floor(Math.random() * objs.length)]);
}

function createStone (svgClass, shapeClass, id, opts) {
  let svg = createCustomElement("svg", svgClass, `${id}-svg`);
  if (Object.keys(opts).indexOf("points") < 0) {
    svg.append(createCircle(shapeClass, `${id}-stone`, opts))
  } else {
    svg.append(createPolygon(shapeClass, `${id}-stone`, opts))
  }
  return(svg);
}

function createPolygon(className, id, opts) {
  let polygon = createCustomElement("polygon", className, id);
  setAttributes(polygon, {
    "fill": opts.color,
    "points": opts.points,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  })
  return(polygon);
}
function createCircle (className, id, opts) {
  let circle = createCustomElement("circle", className, id);
  setAttributes(circle, {
    "cx": opts.cx,
    "cy": opts.cy,
    "r": opts.r,
    "fill": opts.color,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  })
  return(circle);
}
function calcPolygon(input) {
  // Adapted from https://gist.github.com/jonthesquirrel/e2807811d58a6627ded4
  let output = [];
  for (let i = 1; i <= input.n; i++) {
    output.push(
      ((input.r * Math.cos(input.a + 2 * i * Math.PI / input.n)) + len/2).toFixed(0).toString() + "," +
      ((input.r * Math.sin(input.a + 2 * i * Math.PI / input.n)) + len/2).toFixed(0).toString()
    )
  }
  return output.join(" ")
}
function createCustomElement (type = 'div', className, id) {
  let element = (svgElements.indexOf(type) < 0)?
    document.createElement(type):
    document.createElementNS("http://www.w3.org/2000/svg", type);
  if (className.length > 0) element.setAttribute("class", className);
  element.setAttribute("id", id);
  return element;
}
function createDivWithStyle (className = "div", id = "", style = "") {
  let element = createCustomElement('div', className, id);
  setStyle(element, style);
  return element;
}
function createText(h = "h1", text = 'hello') {
  let element = document.createElement(h);
  let tx = document.createTextNode(text);
  element.append(tx);
  return(element)
}
function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}
