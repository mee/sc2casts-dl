sc2casts-dl
===========

A mash-up script of sc2casts and youtube-dl

Building Instructions:
----------------------

Install Cabal/Haskell Platform and youtube-dl, then

$ cabal configure
$ cabal build
$ dist/build/sc2casts-dl/sc2casts-dl

Example use:
------------

	$ sc2casts-dl
	Fetching Feed...Done.
	Found 25 items.
	1:  Dignitas vs NSHoSeo (Best of 9)
	      IPL Team Arena - Qualifier / cast by: CatsPajamas & doa @ YouTube
	2:  Grubby vs Sheth (1 Game)
	     [PvZ] Grubby - Sheth BO11 Series - Showmatch / cast by: Ipp @ YouTube
	3:  Feast vs MMA (1 Game)
	     [PvT] IEM Kiev - Quarter Finals / cast by: Force @ YouTube
	4:  Grubby vs EmpireKas (Best of 3)
	     [PvT] IEM Kiev - Group Stage / cast by: GLiTCH @ YouTube
	5:  Naniwa vs WhiteRa (Best of 3)
	     [PvP] IEM Kiev - Group Stage / cast by: GLiTCH @ YouTube
	6:  HoSeo.Sage vs DeParture (1 Game)
	     [PvZ] Starcraft Ladder - Korean Starcraft Match / cast by: All-In TV @ YouTube
	7:  SlayerS_Brown vs Unknown Player (1 Game)
	     [PvT] Starcraft Ladder - Korean Starcraft Match / cast by: All-In TV @ YouTube
	8:  DeMuslim vs ViBE (1 Game)
	     [TvZ] Unknown Event - Pro SC2 VOD / cast by: Force @ YouTube
	9:  oGsTop vs ReXSwaGGie (1 Game)
	     [TvT] Starcraft Ladder - Korean Starcraft Match / cast by: All-In TV @ YouTube
	10: WhiteRa vs TLO (1 Game)
	     [PvZ] Special Tactics - Live Commentary / cast by: WhiteRa @ YouTube
	11: PsY vs Windir (1 Game)
	     [ZvT] Starcraft Ladder - Own Replay Analysis / cast by: PsyStarcraft @ YouTube
	12: Destiny and Ailuj vs Multiple Players (3 Games)
	      Starcraft Ladder - 4v4 / cast by: Voice Chat @ YouTube
	13: Shine vs Symbol (Best of 5)
	     [ZvZ] Iron Squid - Qualifier / cast by: TotalBiscuit & Apollo @ YouTube
	14: DeMuslim vs Golden (1 Game)
	     [TvZ] Starcraft Ladder - Battle.net VOD / cast by: Husky Starcraft @ YouTube
	15: iNcontroL vs iloveoov (1 Game)
	     [PvZ] Starcraft Ladder - Battle.net VOD / cast by: Husky Starcraft @ YouTube
	16: PoltPrime vs Symbol (Best of 3)
	     [TvZ] Iron Squid - Qualifier / cast by: TotalBiscuit & Apollo @ YouTube
	17: KawaiiRice vs Symbol (Best of 3)
	     [TvZ] Iron Squid - Qualifier / cast by: TotalBiscuit & Apollo @ YouTube
	18: Grubby vs Sheth (Game 5)
	     [PvZ] Grubby - Sheth BO11 Series - Showmatch / cast by: Ipp @ YouTube
	19: WhiteRa vs Duuke (1 Game)
	     [PvZ] Special Tactics - Live Commentary / cast by: WhiteRa @ YouTube
	20: DeMuslim vs vileState (1 Game)
	     [TvP] Unknown Event - Pro SC2 VOD / cast by: Force @ YouTube
	21: EmpireKas vs Zenio (Best of 5)
	     [TvZ] IEM Kiev - 3rd-4th / cast by: Crota @ YouTube
	22: PsY vs iSPanther (1 Game)
	     [ZvZ] Starcraft Ladder - Own Replay Analysis / cast by: PsyStarcraft @ YouTube
	23: Drewbie vs DMmalstryx (1 Game)
	     [TvZ] Starcraft Ladder - Battle.net VOD / cast by: barney @ YouTube
	24: LiquidHero vs PoltPrime (Best of 7)
	     [PvT] NASL Sunday Showdown - Showmatch / cast by: Gretorp & Sheth @ YouTube
	25: MarineKingPrime vs oGsMC (Best of 3)
	     [TvP] HomeStory Cup - Group Stage / cast by: Wyrd @ YouTube
	Fetch: 7 5
	Fetching 2 Items.
	Fetching Naniwa vs WhiteRa (Best of 3)...Done.
	Fetching SlayerS_Brown vs Unknown Player (1 Game)...Done.
	Finished.

	$ ls *.mp4
	NaNiwa_P_v_WhiteRa_P_G1_from_IEM_Kiev-vttMC9hcHUw.mp4  SlayerS_Brown_vs_뽀통령_PvT-l9uMKBnmmos.mp4
	NaNiwa_P_v_WhiteRa_P_G2_from_IEM_Kiev-TmCnSyrg76s.mp4

