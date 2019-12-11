::oo::class create ::loon::classes::Worldview_Controller {
     
    superclass ::loon::classes::ZoomPan_Controller
    
    
    method setModel {Model} {
	my variable map
	next $Model
	$map setModel $Model
    }
    
    method init {} {
	my variable view canvas
	
	next
	
	## pan also with left button
	bind $canvas <Button1-Motion> "+[self] pan %x %y both"
	bind $canvas <Control-Button1-Motion> "+[self] pan %x %y x"
	bind $canvas <Shift-Button1-Motion> "+[self] pan %x %y y"
	
    }



}
