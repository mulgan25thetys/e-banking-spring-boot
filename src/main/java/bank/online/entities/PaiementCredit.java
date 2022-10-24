package bank.online.entities;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
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
public class PaiementCredit implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Id
	@Column(name = "idpaiement")
	private Long idPaiement;
	private Float restant_du;
	private Float interet;
	private Float amortissement;
	private Float mensualite;
	private Boolean aRembourse;
	@Temporal(TemporalType.DATE)
	private  Date dateRemboursement;
	@Temporal(TemporalType.DATE)
	private Date dateAjout;
	@Temporal(TemporalType.DATE)
	private Date dateLimit;

	@JsonIgnore
	@ManyToOne
	private Credit credit;

	public PaiementCredit() {
		super();
	}

	public PaiementCredit(Float restant_du, Float interet, Float amortissement, Float mensualite, Boolean aRembourse,
			Date dateLimit) {
		super();
		this.restant_du = restant_du;
		this.interet = interet;
		this.amortissement = amortissement;
		this.mensualite = mensualite;
		this.aRembourse = aRembourse;
		this.dateLimit = dateLimit;
	}
	
	
	
}
