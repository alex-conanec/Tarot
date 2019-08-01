
CREATE SEQUENCE public.goulash_id_seq;

CREATE TABLE public.Goulash (
                id INTEGER NOT NULL DEFAULT nextval('public.goulash_id_seq'),
                Date DATE NOT NULL,
                Heure TIME,
                CONSTRAINT goulash_pk PRIMARY KEY (id)
);


ALTER SEQUENCE public.goulash_id_seq OWNED BY public.Goulash.id;

CREATE TABLE public.Joueur (
                Pseudo VARCHAR(50) NOT NULL,
                Nom VARCHAR(50) NOT NULL,
                Prenom VARCHAR(50) NOT NULL,
                email VARCHAR(100) NOT NULL,
                CONSTRAINT joueur_pk PRIMARY KEY (Pseudo)
);


CREATE TABLE public.Scores_goulash (
                partie_goulash_id INTEGER NOT NULL,
                joueur_id VARCHAR(50) NOT NULL,
                Score NUMERIC(3,1) NOT NULL,
                CONSTRAINT scores_goulash_pk PRIMARY KEY (partie_goulash_id, joueur_id)
);


CREATE SEQUENCE public.partie_tarot_id_seq;

CREATE TABLE public.Partie_tarot (
                id INTEGER NOT NULL DEFAULT nextval('public.partie_tarot_id_seq'),
                Date DATE NOT NULL,
                Heure TIME,
                Contrat VARCHAR(20) NOT NULL,
                Bouts INTEGER NOT NULL,
                Points NUMERIC(3,1) NOT NULL,
                nb_joueur INTEGER,
                preneur VARCHAR(50) NOT NULL,
                appele VARCHAR(50) NOT NULL,
                couleur VARCHAR(20) NOT NULL,
                marque INTEGER NOT NULL,
                CONSTRAINT partie_tarot_pk PRIMARY KEY (id)
);


ALTER SEQUENCE public.partie_tarot_id_seq OWNED BY public.Partie_tarot.id;

CREATE TABLE public.Scores_tarot (
                partie_tarot_id INTEGER NOT NULL,
                joueur_id VARCHAR(50) NOT NULL,
                score INTEGER NOT NULL,
                CONSTRAINT scores_tarot_pk PRIMARY KEY (partie_tarot_id, joueur_id)
);


CREATE TABLE public.Petit_au_bout (
                partie_tarot_id INTEGER NOT NULL,
                Camps VARCHAR(20) NOT NULL,
                Succes BOOLEAN NOT NULL,
                CONSTRAINT petit_au_bout_pk PRIMARY KEY (partie_tarot_id)
);


CREATE TABLE public.Annonce (
                partie_tarot_id INTEGER NOT NULL,
                joueur_id VARCHAR(50) NOT NULL,
                Type VARCHAR(20) NOT NULL,
                CONSTRAINT annonce_pk PRIMARY KEY (partie_tarot_id, joueur_id, Type)
);


CREATE TABLE public.Defense (
                partie_tarot_id INTEGER NOT NULL,
                joueur_id VARCHAR(50) NOT NULL,
                CONSTRAINT defense_pk PRIMARY KEY (partie_tarot_id, joueur_id)
);


ALTER TABLE public.Scores_goulash ADD CONSTRAINT goulash_scores_goulash_fk
FOREIGN KEY (partie_goulash_id)
REFERENCES public.Goulash (id)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Defense ADD CONSTRAINT joueur_defense_fk
FOREIGN KEY (joueur_id)
REFERENCES public.Joueur (Pseudo)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Partie_tarot ADD CONSTRAINT joueur_partie_tarot_fk
FOREIGN KEY (appele)
REFERENCES public.Joueur (Pseudo)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Partie_tarot ADD CONSTRAINT joueur_partie_tarot_fk1
FOREIGN KEY (preneur)
REFERENCES public.Joueur (Pseudo)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Annonce ADD CONSTRAINT joueur_annonce_fk
FOREIGN KEY (joueur_id)
REFERENCES public.Joueur (Pseudo)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Scores_goulash ADD CONSTRAINT joueur_scores_goulash_fk
FOREIGN KEY (joueur_id)
REFERENCES public.Joueur (Pseudo)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Scores_tarot ADD CONSTRAINT joueur_scores_tarot_fk
FOREIGN KEY (joueur_id)
REFERENCES public.Joueur (Pseudo)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Defense ADD CONSTRAINT partie_tarot_defense_fk
FOREIGN KEY (partie_tarot_id)
REFERENCES public.Partie_tarot (id)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Annonce ADD CONSTRAINT partie_tarot_annonce_fk
FOREIGN KEY (partie_tarot_id)
REFERENCES public.Partie_tarot (id)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Petit_au_bout ADD CONSTRAINT partie_tarot_petit_au_bout_fk
FOREIGN KEY (partie_tarot_id)
REFERENCES public.Partie_tarot (id)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;

ALTER TABLE public.Scores_tarot ADD CONSTRAINT partie_tarot_scores_tarot_fk
FOREIGN KEY (partie_tarot_id)
REFERENCES public.Partie_tarot (id)
ON DELETE CASCADE
ON UPDATE CASCADE
NOT DEFERRABLE;
