SELECT d.id,
    d.date_reception_dpe,
    d.desactive,
    d.dpe_remplacant_id,
    r.date_reception_dpe AS date_reception_dpe_remplacant,
    (EXTRACT(epoch FROM r.date_reception_dpe - d.date_reception_dpe) / 86400::numeric)::double precision AS interval_dpe_remplacant,
    d.ancien_dpe_id,
    ancien.date_reception_dpe AS date_reception_ancien_dpe,
    (EXTRACT(epoch FROM d.date_reception_dpe - ancien.date_reception_dpe) / 86400::numeric)::double precision AS interval_ancien_dpe,
    deco.classe_bilan_dpe AS classe_dpe,
    deg.classe_emission_ges,
    deco.ep_conso_5_usages_m2,
    deco.ep_conso_5_usages / deco.ep_conso_5_usages_m2 AS surface_conso,
    deco.ep_conso_ch / (deco.ep_conso_5_usages / deco.ep_conso_5_usages_m2) AS conso_chauffage,
    deco.ep_conso_ecs / (deco.ep_conso_5_usages / deco.ep_conso_5_usages_m2) AS conso_ecs,
    deco.ep_conso_eclairage / (deco.ep_conso_5_usages / deco.ep_conso_5_usages_m2) AS conso_eclairage,
    deco.ep_conso_fr / (deco.ep_conso_5_usages / deco.ep_conso_5_usages_m2) AS conso_froid,
    deco.ep_conso_totale_auxiliaire / (deco.ep_conso_5_usages / deco.ep_conso_5_usages_m2) AS conso_aux,
    deg.emission_ges_5_usages_m2 AS conso_ges,
    ef.conso_5_usages_m2 AS conso_ef,
    dta.ban_postcode AS code_postal,
    a.date_visite_diagnostiqueur,
    a.enum_version_id AS version,
    cp.lib AS chauffage_principal,
    ec.lib AS eau_chaude_sanitaire,
    a.date_fin_validite_dpe,
    dcg.surface_habitable_logement,
    zc.lib AS zone_climat,
    dta.ban_city AS ville,
    dta.ban_departement AS departement,
    dta.ban_region AS region,
    dcg.hsp,
    pc.lib AS periode_construction,
        CASE
            WHEN ma.lib ~~ '%appartement%'::text THEN 'appartement'::text
            WHEN ma.lib ~~ '%maison individuelle%'::text THEN 'maison individuelle'::text
            ELSE NULL::text
        END AS type_logement
FROM dpe d
    LEFT JOIN dpe r ON d.dpe_remplacant_id = r.id
    LEFT JOIN dpe ancien ON d.ancien_dpe_id = ancien.id
    LEFT JOIN dpe_enum_type_energie cp ON d.enum_type_energie_chauffage_principal_id = cp.id
    LEFT JOIN dpe_enum_type_energie ec ON d.enum_type_energie_ecs_principal_id = ec.id
    JOIN dpe_administratif a ON d.id = a.dpe_id
    JOIN dpe_geolocalisation geo ON d.id = geo.administratif_id
    JOIN dpe_t_adresse dta ON geo.adresse_bien_id = dta.id
    JOIN dpe_emission_ges deg ON d.id = deg.dpe_id
    JOIN dpe_ep_conso deco ON d.id = deco.dpe_id
    JOIN dpe_caracteristique_generale dcg ON dcg.dpe_id = d.id
    JOIN dpe_enum_periode_construction pc ON dcg.enum_periode_construction_id = pc.id
    JOIN dpe_ef_conso ef ON ef.dpe_id = d.id
    JOIN dpe_enum_methode_application_dpe_log ma ON dcg.enum_methode_application_dpe_log_id = ma.id
    JOIN dpe_meteo meteo ON meteo.dpe_id = d.id
    JOIN dpe_enum_zone_climatique zc ON zc.id = meteo.enum_zone_climatique_id
    JOIN dpe_enum_classe_altitude ca ON ca.id = meteo.enum_classe_altitude_id
    JOIN dpe_enum_modele_dpe mod ON mod.id = a.enum_modele_dpe_id
WHERE d.statut IS NULL AND (ma.lib ~~ '%appartement%'::text OR ma.lib ~~ '%maison individuelle%'::text) AND NOT (d.identifiant_dpe::text IN ( SELECT DISTINCT dpe_administratif.dpe_immeuble_associe
        FROM dpe_administratif
        WHERE dpe_administratif.dpe_immeuble_associe IS NOT NULL)) 
        AND deco.ep_conso_5_usages_m2 >= 10::double precision 
        AND deco.ep_conso_5_usages_m2 <= 800::double precision 
        AND dcg.enum_usage_fonctionnel_batiment_id IS NULL 
        AND dcg.enum_categorie_erp_id IS NULL 
        AND deco.ep_conso_5_usages <> 'NaN'::double precision 
        AND deco.ep_conso_ecs <> 'NaN'::double precision 
        AND deco.ep_conso_ch <> 'NaN'::double precision 
        AND deco.ep_conso_eclairage <> 'NaN'::double precision 
        AND deco.ep_conso_fr <> 'NaN'::double precision 
        AND deco.ep_conso_totale_auxiliaire <> 'NaN'::double precision 
        AND abs(deco.ep_conso_5_usages - (deco.ep_conso_ecs + deco.ep_conso_ch + deco.ep_conso_eclairage + deco.ep_conso_fr + deco.ep_conso_totale_auxiliaire)) < 0.01::double precision 
        AND (ca.lib <> 'supérieur à 800m'::text OR (zc.lib <> ALL (ARRAY['H1b'::text, 'H1c'::text, 'H2d'::text]))) 
        AND mod.lib = 'DPE 3CL 2021 méthode logement'::text 
        AND a.enum_version_id::text ~~ '2%'::text;
