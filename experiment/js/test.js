
let t1 = createSvg("test", 't1');
let t2 = createSvg("test", 't2');


t1.append(createCircle("shape", 'c1', {
  cx: "40",
  cy: "40",
  r: "35",
  color: "black",
  hasBorder: true,
}));
t2.append(createPolygon("shape", 'p1', {
  color: "limegreen",
  points: "40,5 5,75 75,75",
  hasBorder: true,
}))


document.body.append(t1);
document.body.append(t2);


function createPolygon(className, id, opts) {
  let polygon = document.createElementNS("http://www.w3.org/2000/svg", "polygon");
  setAttributes(polygon, {
    "class": className,
    "id": id,
    "fill": opts.color,
    "points": opts.points,
    "stroke-width": opts.hasBorder? "8px" : "0px",
  })
  return(polygon);
}

function createCircle (className, id, opts) {
  let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
  setAttributes(circle, {
    "class": className,
    "id": id,
    "cx": opts.cx,
    "cy": opts.cy,
    "r": opts.r,
    "fill": opts.color,
    "stroke-width": opts.hasBorder? "8px" : "0px",
  })
  return(circle);
}

function createSvg (className, id) {
  let svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("class", className);
  svg.setAttribute("id", id);
  return(svg);
}

function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}
