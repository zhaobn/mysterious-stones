
const borderWidth = "8px";
const mar = 5;
const len = 80;

let t1 = createSvg("test", `t1`);
let t2 = createSvg("test", `t2`);
let t3 = createSvg("test", `t3`);
let t4 = createSvg("test", `t4`);
let t5 = createSvg("test", `t5`);
let t6 = createSvg("test", `t6`);
let t7 = createSvg("test", `t7`);
let t8 = createSvg("test", `t8`);


t1.append(createCircle("shape", 's1', {
  cx: "40",
  cy: "40",
  r: "35",
  color: "black",
  hasBorder: true,
}));
t2.append(createPolygon("shape", 's2', {
  color: "limegreen",
  points: `
    ${len/2},${mar}
    ${mar},${len-mar}
    ${len-mar},${len-mar}
  `,
  hasBorder: false,
}));
t3.append(createPolygon("shape", 's3', {
  color: "limegreen",
  points: `
    ${mar},${mar}
    ${mar},${len-mar}
    ${len-mar},${len-mar}
    ${len-mar},${mar}
    `,
  hasBorder: false,
}))
t4.append(createPolygon("shape", 's4', {
  color: "limegreen",
  points: calcPolygon({n:6,r:40,a:0}),
  //{n:5,r:35,a:60}
  hasBorder: false,
}))



document.body.append(t1);
document.body.append(t2);
document.body.append(t3);
document.body.append(t4);



function createPolygon(className, id, opts) {
  let polygon = createSvgElement("polygon");
  setAttributes(polygon, {
    "class": className,
    "id": id,
    "fill": opts.color,
    "points": opts.points,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  })
  return(polygon);
}

function createCircle (className, id, opts) {
  let circle = createSvgElement("circle");
  setAttributes(circle, {
    "class": className,
    "id": id,
    "cx": opts.cx,
    "cy": opts.cy,
    "r": opts.r,
    "fill": opts.color,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  })
  return(circle);
}

function createSvg (className, id) {
  let svg = createSvgElement("svg");
  svg.setAttribute("class", className);
  svg.setAttribute("id", id);
  return(svg);
}

function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}

function createSvgElement (el) {
  let svgEl = document.createElementNS("http://www.w3.org/2000/svg", el);
  return(svgEl);
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
