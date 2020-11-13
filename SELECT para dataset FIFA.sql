SELECT id, 
        
		COALESCE(name,'N/A') as name,
        COALESCE(age,-1) as age,
		COALESCE(nationality,'N/A') as nationality,
		COALESCE(club,'N/A') as club,
		COALESCE(value,'N/A') as value,
		COALESCE(wage,'N/A') as wage,
		COALESCE(release_clause,'N/A') as release_clause,

		overall,
        cast(weak_foot as int)*20 as weak_foot,
	    cast(skill_moves as int)*20 as skill_moves,
	    case when work_rate like '%High%' then 90 
	         when work_rate like '%Medium%' then 60
	         when  work_rate like '%Low%' then 30
		else 0 
		end as work_rate,
	    case when "position" = 'GK' then 1
		
		      when "position" = 'CB' then 10
			  when "position" = 'LCB' then 10
			  when "position" = 'RCB' then 10
			  
			  when "position" = 'RB' then 20
			  when "position" = 'LB' then 25
			  
			  when "position" = 'RWB' then 30
			  when "position" = 'LWB' then 35
			  
			  when "position" = 'CDM' or "position" ='LDM' or "position" = 'RDM' then 40
			  
			  when "position" = 'CM' or "position" ='LCM' or "position" = 'RCM' then 50
			  
			  when "position" = 'RM' then 60
			  when "position" = 'LM' then 65
			  
			  when "position" = 'CAM' or "position" ='RAM' or "position" = 'LAM' then 70
			  
			  when "position" = 'CF' or "position" = 'RW' or "position" = 'LW' then 80
			  
			  when "position" = 'RF' or "position" = 'RS' then 85
			  
			  when "position" = 'LF' or "position" = 'LS' then 90
			  
			  when "position" = 'ST' then 100
			  
			  else 0 end as "position",

       crossing, finishing, headingaccuracy, shortpassing, 
	   volleys, dribbling, curve, fkaccuracy, longpassing, ballcontrol, acceleration, sprintspeed, agility, reactions, 
	   balance, shotpower, jumping, stamina, strength, longshots, aggression, interceptions, positioning, vision, 
	   penalties, composure, marking, standingtackle, slidingtackle, gkdiving, gkhandling, gkkicking, gkpositioning, 
	   gkreflexes
	FROM public.jugadores;




	
	