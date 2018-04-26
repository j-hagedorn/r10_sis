# devtools::install_github("timelyportfolio/d3vennR")

library("d3vennR")

venn_tooltip <- function( venn ){
  venn$x$tasks[length(venn$x$tasks)+1] <- list(
    htmlwidgets::JS('
                    function(){
                    var div = d3.select(this);
                    
                    // add a tooltip
                    var tooltip = d3.select("body").append("div")
                    .attr("class", "venntooltip")
                    .style("position", "absolute")
                    .style("text-align", "center")
                    .style("width", 128)
                    .style("height", 16)
                    .style("background", "#333")
                    .style("color","#ddd")
                    .style("padding","2px")
                    .style("border","0px")
                    .style("border-radius","8px")
                    .style("opacity",0);
                    
                    div.selectAll("path")
                    .style("stroke-opacity", 0)
                    .style("stroke", "#fff")
                    .style("stroke-width", 0)
                    
                    // add listeners to all the groups to display tooltip on mousover
                    div.selectAll("g")
                    .on("mouseover", function(d, i) {
                    
                    // sort all the areas relative to the current item
                    venn.sortAreas(div, d);
                    
                    // Display a tooltip with the current size
                    tooltip.transition().duration(400).style("opacity", .9);
                    tooltip.text(d.size);
                    
                    // highlight the current path
                    var selection = d3.select(this).transition("tooltip").duration(400);
                    selection.select("path")
                    .style("stroke-width", 3)
                    .style("fill-opacity", d.sets.length == 1 ? .4 : .1)
                    .style("stroke-opacity", 1);
                    })
                    
                    .on("mousemove", function() {
                    tooltip.style("left", (d3.event.pageX) + "px")
                    .style("top", (d3.event.pageY - 28) + "px");
                    })
                    
                    .on("mouseout", function(d, i) {
                    tooltip.transition().duration(400).style("opacity", 0);
                    var selection = d3.select(this).transition("tooltip").duration(400);
                    selection.select("path")
                    .style("stroke-width", 0)
                    .style("fill-opacity", d.sets.length == 1 ? .25 : .0)
                    .style("stroke-opacity", 0);
                    });
                    }
                    ')
  )
  venn
}

id_lists <-
  list(
    elig_svs = unique(elig_svs$MEDICAID_ID[elig_svs$eligible_svs == T]),
    elig_qi = unique(elig_qi$MEDICAID_ID),
    sis_ids = unique(sis_ids$mcaid_id),
    sis_defer = unique(sis_defer$MEDICAID_ID)
  )

tst <-
  list(
    list("sets" = list(0), "label" = "Eligible Services", "size" = n_distinct(id_lists$elig_svs)),
    list("sets" = list(1), "label" = "Eligible QI", "size" = n_distinct(id_lists$elig_qi)),
    list("sets" = list(2), "label" = "Completed SIS", "size" = n_distinct(id_lists$sis_ids)),
    list("sets" = list(3), "label" = "Deferred", "size" = n_distinct(id_lists$sis_defer)),
    list("sets" = list(0, 1), "size" = length(intersect(id_lists$elig_svs,id_lists$elig_qi))),
    list("sets" = list(0, 2), "size" = length(intersect(id_lists$elig_svs,id_lists$sis_ids))),
    list("sets" = list(0, 3), "size" = length(intersect(id_lists$elig_svs,id_lists$sis_defer))),
    list("sets" = list(1, 2), "size" = length(intersect(id_lists$elig_qi,id_lists$sis_ids))),
    list("sets" = list(1, 3), "size" = length(intersect(id_lists$elig_qi,id_lists$sis_defer))),
    list("sets" = list(2, 3), "size" = length(intersect(id_lists$sis_ids,id_lists$sis_defer))),
    list(
      "sets" = list(0, 1, 2), 
      "size" = length(Reduce(intersect,list(id_lists$elig_svs,id_lists$elig_qi,id_lists$sis_ids)))
    ),
    list(
      "sets" = list(0, 1, 3), 
      "size" = length(Reduce(intersect,list(id_lists$elig_svs,id_lists$elig_qi,id_lists$sis_defer)))
    ),
    list(
      "sets" = list(0, 2, 3), 
      "size" = length(Reduce(intersect,list(id_lists$elig_svs,id_lists$sis_ids,id_lists$sis_defer)))
    ),
    list(
      "sets" = list(1, 2, 3), 
      "size" = length(Reduce(intersect,list(id_lists$elig_qi,id_lists$sis_ids,id_lists$sis_defer)))
    ),
    list(
      "sets" = list(0, 1, 2, 3), 
      "size" = length(Reduce(intersect,list(id_lists$elig_svs,id_lists$elig_qi,id_lists$sis_ids,id_lists$sis_defer)))
    )
  )

tst %>%
  d3vennR() %>%
  venn_tooltip()
