package bank.online.entities;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Proxy;

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
public class ContratAssurance implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long idContratAssurance;
	@Enumerated(EnumType.STRING)
	private NatureAssurance nature;
	@Enumerated(EnumType.STRING)
	private CategorieAssurance categorie;
	@Enumerated(EnumType.STRING)
	private DureeContratAssurance duree;
	@Enumerated(EnumType.STRING)
	private TypeSubscription type;
	@Enumerated(EnumType.STRING)
	private EtatContrat etat;
	private float prime;
	private float versement;
	private Boolean estResilie;
	@Temporal(TemporalType.DATE)
	private Date dateResiliation;
	@Temporal(TemporalType.DATE)
	private Date dateCreation;
	@Temporal(TemporalType.DATE)
	private Date dateModification;
	
	@OneToMany(cascade = CascadeType.ALL,fetch = FetchType.LAZY)
	private List<QuestionnaireAssurance> questionnaires;

	public ContratAssurance() {
		super();
	}
	
}

