library(plotly)
library(htmlwidgets)
library(ggplot2)

data <- data.frame(
    Time = round(runif(10), 2),
    Value = round(runif(10), 2),
    Type = rep(c("A", "B"), each = 5)
    )

gg <- ggplot(data = data) +
    theme(legend.title = element_blank()) +
    geom_point(mapping = aes(x = Time, y = Value, colour = Type), size = 2)

# http://svgicons.sparkk.fr/
icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"

dl_button <- list(
    name = "Download data",
    icon = list(
        path = icon_svg_path,
        transform = "scale(0.84) translate(-1, -1)"
        ),
    click = htmlwidgets::JS("
          function(gd) {
            var text = '';
            for(var i = 0; i < gd.data.length; i++){
              text += gd.layout.xaxis.title.text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
              text += gd.layout.yaxis.title.text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
            };
            var blob = new Blob([text], {type: 'text/plain'});
            var a = document.createElement('a');
            const object_URL = URL.createObjectURL(blob);
            a.href = object_URL;
            a.download = 'data.csv';
            document.body.appendChild(a);
            a.click();
            URL.revokeObjectURL(object_URL);
          }
   ")
)

ggplotly(gg) %>%
    layout(legend = list(y = 0.5)) %>%
    config(modeBarButtonsToAdd = list(dl_button))