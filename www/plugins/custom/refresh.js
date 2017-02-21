$(document).read(function () {
	$(document).on('click', '.refresher', function (){
		$.ajax({
			url: 'setthemcookies',
			method: get
			dataType: 'html'
			success: function(response){
				$('#itemlist').html(response);
} }); }); });