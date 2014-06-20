(setq vm-auto-folder-case-fold-search t
      vm-auto-folder-alist
      '(("Subject"
	 ("\\[trikes]" . "trike")
	 ("\\[OLRG]" . "land-rovers")
	 ("coin" . "coin")
	 ("tssf" . "TSSF")
	 ("\\[waterways]" . "junk")
	 ("versor" . "versor")
	 ("joylisp" . "versor")
	 ("gamepad" . "versor")
	 ("ocd" . "web-pages")
	 ("\\[Ely" . "ely")
	 )

	("From"
	 ("Emma.Abbey" . "EmmaA")
	 ("Nicola Sanders" . "Nibbie")
	 ("Annaik.Genson" . "Annaik")
	 ("nicola.sanders" . "Nibbie")
	 ("christine.titmus" . "Christine")
	 ("christine titmus" . "Christine")
	 ("emma.parry" . "Emma.Parry")
	 ("sturdy" . "family")
	 ("sjk@justus.anglican.org" .
	  '(("Subject"
	     ("camgsm access log entries" . "/tmp/camgsm-logs"))))

	 ("xc_build@xensource.com" . (substitute-in-file-name "~/workmail/builds"))
	 )

	("To"
	 ("Emma.Abbey" . "EmmaA")
	 ("Nicola Sanders" . "Nibbie")
	 ("Nibbie Sanders" . "Nibbie")
	 ("nicola.sanders" . "Nibbie")
	 ("sturdy" . "family")
	 ("Annaik.Genson" . "Annaik")
	 ("emma.parry" . "Emma.Parry")
	 ("christine.titmus" . "Christine")
	 ("christine titmus" . "Christine")
	 )

	 ("To\\|Cc"
	  ("<XenClientNotify@citrix.com>" .
	  '(("Subject"
	     ("\\[.+\\.git]" . (substitute-in-file-name "~/workmail/checkins")))))

	 ("<client@virtualcomputer.com>" .
	  '(("Subject"
	     ("WFH" . (substitute-in-file-name "~/workmail/WFHs"))
	     ("putback to" . (substitute-in-file-name "~/workmail/putbacks"))
	     ("Re: Review:" . (substitute-in-file-name "~/workmail/reviews"))
	     ("putback to" . (substitute-in-file-name "~/workmail/putbacks"))
	     ("Daily putbacks to" . (substitute-in-file-name "~/workmail/putbacks")))))
	 )
	)
      )
