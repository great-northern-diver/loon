
oo::class create loon::classes::Plot3DInspector {
    
    superclass ::loon::classes::PlotInspector

    
    method CheckNewActivewidget {widget} {
        if {![info object isa typeof $widget\
              "::loon::classes::Scatterplot3D_Widget"]} {
            error "$widget is not a scatterplot3D widget."
        }
    }
}
