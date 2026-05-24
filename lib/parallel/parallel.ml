let available () = Domain.recommended_domain_count () > 1

let run ~parallel ~sequential =
  if available () then parallel () else sequential ()
