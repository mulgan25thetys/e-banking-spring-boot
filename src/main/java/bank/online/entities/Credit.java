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
	private ModeRemboursement modeRemboursement;
	
	private Float montantDemande;
	private Float montantMensuel;
	private Float apportPersonnel;
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

	public Credit(CategorieCredit categorie, ModeRemboursement modeRemboursement, Float montantDemande,
			Float montantMensuel, Float montantTransaction, Integer duree, Date echeance, String description,
			List<PaiementCredit> paiements) {
		super();
		this.categorie = categorie;
		this.modeRemboursement = modeRemboursement;
		this.montantDemande = montantDemande;
		this.montantMensuel = montantMensuel;
		this.montantTransaction = montantTransaction;
		this.duree = duree;
		this.echeance = echeance;
		this.description = description;
		this.paiements =paiements;
	}
	
	
	
}

