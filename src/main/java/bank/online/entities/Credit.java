package bank.online.entities;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Proxy;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.FieldDefaults;

@Entity
@Proxy(lazy=false)
@Getter
@Setter
@FieldDefaults(level = AccessLevel.PRIVATE)
@ToString
@Builder
@AllArgsConstructor
public class Credit implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Id
	@Column(name = "idcredit")
	private Long idCredit;
	
	@Enumerated(EnumType.STRING)
	private CategorieCredit categorie;
	
	@Enumerated(EnumType.STRING)
	private TypeSimulation typeSimulation;
	
	private Float montantDemande;
	private Float montantMensuel;
	private Float apportPersonnel;
	private Float salaireNetPersonnel;
	private Float primeAnnuelle;
	private Float autresRevenus;
	private Float montantTransaction;
	private Integer duree;
	@Temporal(TemporalType.DATE)
	private  Date echeance;
	@Temporal(TemporalType.DATE)
	private  Date dateAjout;
	@Temporal(TemporalType.DATE)
	private Date dateModification;
	private Boolean rembourse;
	private String description;
	@Enumerated(EnumType.STRING)
	private StatusCredit status;
	private Boolean estAccorde;
	private Boolean inUse;
	
	@OneToMany(cascade = CascadeType.ALL,fetch = FetchType.LAZY)
	private List<Attachements> fichiers;
	
	
	@JsonIgnore
	@OneToOne
	private User emprunteur;
	
	@OneToMany(cascade = CascadeType.ALL,fetch = FetchType.EAGER,mappedBy = "credit")
	private List<PaiementCredit> paiements;
	
	@OneToOne
	private ProjetImmobilier projet;

	public Credit() {
		super();
	}

	public Credit(CategorieCredit categorie, TypeSimulation typeSimulation, Float montantDemande,
			Float montantMensuel, Float montantTransaction, Integer duree, Date echeance, String description,
			List<PaiementCredit> paiements,StatusCredit status,Boolean accorde) {
		super();
		this.categorie = categorie;
		this.typeSimulation = typeSimulation;
		this.montantDemande = montantDemande;
		this.montantMensuel = montantMensuel;
		this.montantTransaction = montantTransaction;
		this.duree = duree;
		this.echeance = echeance;
		this.description = description;
		this.paiements =paiements;
		this.status =status;
		this.estAccorde = accorde;
	}
	
	public Credit(CategorieCredit categorie, TypeSimulation typeSimulation, Float montantDemande,
			Float montantMensuel, Float montantTransaction, Integer duree, Date echeance, String description,
			List<PaiementCredit> paiements,StatusCredit status,Float apportPersonnel,Float salaireNetPersonnel,
			Float primeAnnuelle,Float autresRevenus,Boolean accorde) {
		super();
		this.categorie = categorie;
		this.typeSimulation = typeSimulation;
		this.montantDemande = montantDemande;
		this.montantMensuel = montantMensuel;
		this.montantTransaction = montantTransaction;
		this.duree = duree;
		this.echeance = echeance;
		this.description = description;
		this.paiements =paiements;
		this.status =status;
		this.apportPersonnel = apportPersonnel;
		this.salaireNetPersonnel = salaireNetPersonnel;
		this.primeAnnuelle =primeAnnuelle;
		this.autresRevenus =autresRevenus;
		this.estAccorde = accorde;
	}
	
	
	
}

