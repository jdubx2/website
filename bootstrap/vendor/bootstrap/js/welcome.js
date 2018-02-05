


d3.select('#svg8')
  .attr('width', $(window).width() * .55)
  .attr('height', $(window).width() * .55 * .25);


let repeat = () => {
  var path = d3.selectAll('.svg_letter')
  .attr('fill', 'rgba(16,16,16,0)')
  // .attr('stroke', 'rgba(16,16,16,0)');
  .attr('stroke', '#2C7873');

  var totalLength = path.node().getTotalLength();

  path.attr("stroke-dasharray", totalLength + " " + totalLength)
    .attr("stroke-dashoffset", totalLength)
    .transition()
      .attr('stroke', '#2C7873')
      .duration(1500)
      .ease(d3.easeLinear)
      .attr("stroke-dashoffset", 0)
    .transition()
      .duration(1000)
      .attr('fill', 'rgba(16,16,16,1)')
      .ease(d3.easeLinear)
      .transition()
      .delay(7000)
        .duration(2000)
        .attr('fill', 'rgba(16,16,16,0)')
        .attr('stroke', 'rgba(16,16,16,0)')
        .on("end", repeat);

    var line = d3.selectAll('.underline')
    var lineLength = line.node().getTotalLength();

    line.attr("stroke-dasharray", lineLength + " " + lineLength)
      .attr("stroke-dashoffset", lineLength)
      .attr('stroke', '#2C7873')
      .attr('fill', 'rgba(16,16,16,1)')
      .transition()
        .delay(1500)
        .duration(1000)
        .attr("stroke-dashoffset", 0)
      .transition()
        .delay(7000)
        .duration(2000)
        .attr('fill', 'rgba(16,16,16,0)')
        .attr('stroke', 'rgba(16,16,16,0)')
        .on("end", repeat);
  }
$(document).ready(repeat());
