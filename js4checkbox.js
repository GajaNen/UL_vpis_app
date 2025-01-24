$(document).ready(function(){
  $('input[name=var2]').on('click', function(event){
    if($('input[name=var2]:checked').length > 1){
      $(this).prop('checked', false);
    }
    if($('input[name=var1]:checked') == $('input[name=var2]:checked')){
      $(this).prop('checked', false);
    }
  });
});